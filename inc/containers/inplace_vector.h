
#ifndef INPLACE_VECTOR_H
#define INPLACE_VECTOR_H
#pragma once
#include <iterator>
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <limits>
#include <concepts>
#include <type_traits>

namespace urlicht {

    namespace details { // Implementation details, not expected to be used directly

    template <typename Rng, typename T>
    concept CompatibleRange = std::ranges::forward_range<Rng>
        && std::convertible_to<std::ranges::range_reference_t<Rng>, T>;

    // Complying to the standard
    template <std::size_t N>
    using adaptive_size_type =
        std::conditional_t<N <= std::numeric_limits<uint8_t>::max(), uint8_t,
            std::conditional_t<N <= std::numeric_limits<uint16_t>::max(), uint16_t,
                std::conditional_t<N <= std::numeric_limits<uint32_t>::max(), uint32_t,
                    std::conditional_t<N <= std::numeric_limits<uint64_t>::max(), uint64_t, std::size_t>
                >
            >
        >;

    template<typename T, std::size_t N = 0>
    struct zero_sized_storage {
        static_assert(N == 0, "The size of zero_sized_storage must be zero");

        using size_type = uint8_t;
        constexpr zero_sized_storage() noexcept = default;
        constexpr zero_sized_storage(const zero_sized_storage&) noexcept = default;
        constexpr zero_sized_storage(zero_sized_storage&&) noexcept = default;
        constexpr zero_sized_storage& operator=(const zero_sized_storage&) noexcept = default;
        constexpr zero_sized_storage& operator=(zero_sized_storage&&) noexcept = default;
        ~zero_sized_storage() noexcept = default;

    protected:
        [[nodiscard]] static constexpr T* data() noexcept { return nullptr; }
        [[nodiscard]] static constexpr size_type size() noexcept { return 0U; }
        static constexpr void unchecked_set_size(std::size_t size = 0) noexcept {
            if (size) __builtin_unreachable();
        }
    };

    // storage for trivial types, using std::array as the underlying buffer
    template<typename T, std::size_t N>
    struct trivial_storage {
        static_assert((N > 0), "Use zero_sized_storage for N == 0.");
        static_assert(std::is_trivial_v<T>, "The storage type of trivial_storage must be trivial.");

        using size_type = adaptive_size_type<N>;
        using storage_type = std::conditional_t<std::is_const_v<T>, const std::array<std::remove_const_t<T>, N>,
            std::array<T, N>>;
    private:
        alignas(alignof(T)) storage_type storage_;
        size_type size_{0U};
    public:
        constexpr trivial_storage() noexcept = default;
        constexpr trivial_storage(const trivial_storage&) noexcept = default;
        constexpr trivial_storage(trivial_storage&&) noexcept = default;

        constexpr trivial_storage& operator=(const trivial_storage&) noexcept
        requires (!std::is_const_v<T>) = default;

        constexpr trivial_storage& operator=(trivial_storage&&) noexcept
        requires (!std::is_const_v<T>) = default;

        constexpr explicit trivial_storage(size_type size) noexcept
        : size_{size} {
            std::memset(storage_.data(), 0, sizeof(T) * size_);
        }

        constexpr explicit trivial_storage(size_type size, const T& value) noexcept
        : size_{size} {
            if constexpr (sizeof(T) == 1)
                std::memset(storage_.data(), value, sizeof(T) * size_);
            else
                std::fill(storage_.data(), storage_.data() + size_, value);
        }

        // Iterator constructor
        template <std::forward_iterator Iter>
        constexpr trivial_storage(Iter first, Iter last) noexcept
        requires std::convertible_to<std::iter_reference_t<Iter>, T> // Implicit conversion for trivial types
        : size_{0U} {
            unchecked_append_range(first, last);
        }

        // Range constructor
        template <CompatibleRange<T> Rng>
        constexpr explicit trivial_storage(Rng&& rng) noexcept
        : size_{0U} {
            unchecked_append_range(std::forward<Rng>(rng));
        }

        // Initializer list constructor
        template <typename VTy>
        requires std::convertible_to<VTy, T>
        constexpr explicit trivial_storage(std::initializer_list<VTy> list) noexcept
        : trivial_storage(list.begin(), list.end()) { }

        template<std::forward_iterator Iter>
        constexpr void unchecked_append_range(Iter first, Iter last) noexcept // UB if out-of-bound
        requires std::convertible_to<std::iter_reference_t<Iter>, T> {
            const auto m = std::ranges::distance(first, last);
            if constexpr (std::contiguous_iterator<Iter>) {
                std::memcpy(storage_.data() + size_, std::to_address(first), m); // constexpr since c++23
            }
            else {
                std::copy_n(first, m, storage_.data() + size_);
            }
            size_ += m;
        }

        template<CompatibleRange<T> Rng>
        constexpr void unchecked_append_range(Rng&& rng) noexcept {
            unchecked_append_range(std::ranges::begin(rng), std::ranges::end(rng));
        }

        constexpr ~trivial_storage() noexcept = default;

        [[nodiscard]] constexpr T* data() noexcept requires(!std::is_const_v<T>) { return storage_.data(); }

        [[nodiscard]] constexpr const T* data() const noexcept { return storage_.data(); }

        [[nodiscard]] constexpr size_type size() const noexcept { return size_; }

        constexpr void unchecked_set_size(size_type new_size) noexcept
        requires (!std::is_const_v<T>) { // Undefined behaviour if new_size > N
            size_ = new_size;
        }

        constexpr void clear() noexcept requires(!std::is_const_v<T>) { size_ = 0U; }
    };

    // Storage for non-trivial types using raw bytes, optimized for potentially trivial constructor/assignment operator
    template <typename T, std::size_t N>
    struct non_trivial_storage {
        static_assert(!std::is_trivial_v<T>, "Use trivial_storage for trivial value types.");
        static_assert((N > 0), "Use zero_sized_storage for N == 0.");

        using size_type = adaptive_size_type<N>;
        using storage_type = std::conditional_t<std::is_const_v<T>, const std::byte[N * sizeof(T)],
            std::byte[N * sizeof(T)]>;

    private:
        alignas(alignof(T)) storage_type storage_{};
        size_type size_{0U};

    public:
        constexpr non_trivial_storage() noexcept = default;

        // Copy constructor
        constexpr non_trivial_storage(const non_trivial_storage& other)
        noexcept(std::is_trivially_copy_constructible_v<T> || std::is_nothrow_copy_constructible_v<T>)
        requires std::copy_constructible<T>{
            if constexpr (std::is_trivially_copy_constructible_v<T>) // Performs bitwise copying for trivially copyable types
                // constexpr upon c++23
                std::memcpy(this->data(), other.data(), other.size() * sizeof(T));
            else    // Otherwise, fall back to element-wise copying.
                // constexpr upon c++26
                std::uninitialized_copy_n(other.data(), other.size() , this->data());
            size_ = other.size();
        }

        // Move constructor
        constexpr non_trivial_storage(non_trivial_storage&& other)
        noexcept (std::is_trivially_move_constructible_v<T> || std::is_nothrow_move_constructible_v<T>)
        requires std::move_constructible<T> {
            if constexpr (std::is_trivially_move_constructible_v<T>) // Bitwise copying
                std::memcpy(this->data(), other.data(), other.size() * sizeof(T));
            else // Otherwise, move all the elements
                std::uninitialized_move_n(other.data(), other.size() , this->data());
            size_ = other.size();
            other.unchecked_set_size(0U);
        }

        // Iterator constructor
        template <std::forward_iterator Iter>
        constexpr non_trivial_storage(Iter first, Iter last)
        requires std::constructible_from<T, std::iter_reference_t<Iter>> {
            unchecked_append_range(first, last);
        }

        // Range constructor
        template <CompatibleRange<T> Rng>
        constexpr explicit non_trivial_storage(Rng&& rng)
        : non_trivial_storage(std::ranges::begin(rng), std::ranges::end(rng)) { }

        // Initializer list constructor
        template <typename VTy>
        requires std::constructible_from<T, VTy>
        constexpr non_trivial_storage(std::initializer_list<VTy> list)
        : non_trivial_storage(list.begin(), list.end()) { }

        // Trivial copy assignment operator
        constexpr non_trivial_storage& operator=(const non_trivial_storage& other) noexcept
        requires std::is_trivially_copy_assignable_v<T> {
            if (this == &other)
                return *this;

            std::memcpy(this->data(), other.data(), other.size() * sizeof(T));
            size_ = other.size();
            return *this;
        }

        // Trivial move assignment operator
        constexpr non_trivial_storage& operator=(non_trivial_storage&& other) noexcept
        requires std::is_trivially_move_assignable_v<T> {
            if (this == &other)
                return *this;
            this->operator=(other); // Delegates to copy assignment operator
            other.unchecked_set_size(0U);
            return *this;
        }

        // Non-trivial copy assignment operator
        constexpr non_trivial_storage& operator=(const non_trivial_storage& other)
        noexcept(std::is_nothrow_copy_assignable_v<T>
            && std::is_nothrow_copy_constructible_v<T>
            && std::is_nothrow_destructible_v<T>)
        requires std::assignable_from<T&, const T&> && (!std::is_trivially_copy_assignable_v<T>) {
            if (this == &other)
                return *this;
            const size_type k = other.size() > size_ ? size_ : other.size();
            std::copy_n(other.data(), k, this->data());
            if (other.size() > size_) {
                std::uninitialized_copy_n(other.data() + k,other.size() - k, this->data() + k);
            }
            else {
                std::destroy_n(this->data() + k, size_ - k);
            }
            size_ = other.size();
            return *this;
        }

        // Non-trivial move assignment operator
        constexpr non_trivial_storage& operator=(non_trivial_storage&& other)
        noexcept(std::is_nothrow_move_assignable_v<T>
            && std::is_nothrow_move_constructible_v<T>
            && std::is_nothrow_destructible_v<T>)
        requires std::assignable_from<T&, T&&> && (!std::is_trivially_move_assignable_v<T>) {
            if (this == &other)
                return *this;
            const size_type k = other.size() > size_ ? size_ : other.size();
            std::move(other.data(), other.data() + k, this->data());
            if (other.size() > size_) {
                std::uninitialized_move_n(other.data() + k,other.size() - k, this->data() + k);
            }
            else {
                std::destroy_n(this->data() + k, size_ - k);
            }
            size_ = other.size();
            other.unchecked_set_size(0U);
            return *this;
        }

        // Append range - UB if out-of-bound
        template <std::forward_iterator Iter>
        constexpr void unchecked_append_range(Iter first, Iter second)
        noexcept(std::is_trivially_copy_constructible_v<T>
                || std::is_trivially_move_constructible_v<T>
                || std::is_nothrow_copy_assignable_v<T>)
        requires std::constructible_from<T&, std::iter_reference_t<Iter>> {
            const auto m = std::ranges::distance(first, second);
            if constexpr (std::is_trivially_copyable_v<T> && std::contiguous_iterator<Iter>) {
                std::memcpy(this->data() + size_, std::to_address(first), m * sizeof(T));
            }
            else if constexpr (std::is_rvalue_reference_v<std::iter_reference_t<Iter>>) {
                std::uninitialized_move_n(first, m, this->data() + size_);
            }
            else {
                std::uninitialized_copy_n(first, m, this->data() + size_);
            }
            size_ += m;
        }

        // Destructor
        constexpr ~non_trivial_storage() noexcept(std::is_nothrow_destructible_v<T>) {
            if constexpr (!std::is_trivially_destructible_v<T>) {
                std::destroy_n(this->data(), size_);
            }
        }

        [[nodiscard]] constexpr T* data() noexcept requires (!std::is_const_v<T>){
            return reinterpret_cast<T*>(storage_);
        }
        [[nodiscard]] constexpr const T* data() const noexcept {
            return reinterpret_cast<const T*>(storage_);
        }
        [[nodiscard]] constexpr size_type size() const noexcept {
            return size_;
        }

        constexpr void unchecked_set_size(std::size_t new_size) noexcept
        requires (!std::is_const_v<T>) {
#ifndef NDEBUG
            assert(new_size <= N && "New size out-of-bound: non_trivial_storage::unchecked_set_size().");
#endif
            size_ = static_cast<size_type>(new_size);
        }

        constexpr void clear() noexcept(std::is_nothrow_destructible_v<T>)
        requires (!std::is_const_v<T>) {
            if constexpr (!std::is_trivially_destructible_v<T>) {
                std::destroy_n(this->data(), size_);
            }
            size_ = 0U;
        }

    };

    template <typename T, size_t N>
    using adaptive_storage_type = std::conditional_t<N == 0, zero_sized_storage<T>,
        std::conditional_t<std::is_trivial_v<T>, trivial_storage<T, N>, non_trivial_storage<T, N>>>;

    } // namespace details

    template <typename T, size_t N>
    requires std::is_object_v<T>
    class inplace_vector {
    private:
        using storage_type = details::adaptive_storage_type<T, N>;
        [[no_unique_address]] storage_type storage_;
    public:
        using value_type = T;
        using size_type = typename storage_type::size_type;
        using difference_type = std::ptrdiff_t;
        using reference = T&;
        using const_reference = const T&;
        using pointer = T*;
        using const_pointer = const T*;
        using iterator = pointer;
        using const_iterator = const_pointer;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        // Constructors
        // Default constructor
        constexpr inplace_vector() noexcept = default;

        // Delegated to the constructors of storage_type
        // Copy constructor
        constexpr inplace_vector(const inplace_vector& other)
        noexcept(std::is_nothrow_copy_constructible_v<storage_type>)
        requires std::copy_constructible<storage_type> = default;

        // Move constructor
        constexpr inplace_vector(inplace_vector&& other)
        noexcept(std::is_nothrow_move_constructible_v<storage_type>)
        requires std::move_constructible<storage_type> = default;

        // Size constructor - default initializes the first {size} elements
        constexpr explicit inplace_vector(size_type size)
        noexcept(noexcept(storage_type(size)))
        requires std::constructible_from<storage_type, size_type> : storage_(size) { }

        // Valued size constructor - fill the first {size} elements with {value}
        constexpr inplace_vector(size_type size, const T& value)
        noexcept(noexcept(storage_type(size, value)))
        requires std::constructible_from<storage_type, size_type, const T&> : storage_(size, value) { }

        // Iterator constructor
        template <std::forward_iterator Iter>
        constexpr inplace_vector(Iter first, Iter last)
        noexcept(noexcept(storage_type(first, last)))
        requires std::constructible_from<storage_type, Iter, Iter> : storage_(first, last) { }

        // Range constructor
        template <details::CompatibleRange<T> Rng>
        constexpr explicit inplace_vector(Rng&& rng)
        noexcept(noexcept(storage_type(std::forward<Rng>(rng))))
        requires std::constructible_from<storage_type, Rng> : storage_(std::forward<Rng>(rng)) { }

        // Initializer list constructor
        template <typename VTy>
        constexpr inplace_vector(std::initializer_list<VTy> init)
        noexcept(noexcept(storage_type(init)))
        requires std::constructible_from<storage_type, std::initializer_list<VTy>> : storage_(init) { }

        // Destructor
        constexpr ~inplace_vector()
        noexcept(std::is_nothrow_destructible_v<storage_type>) = default;

        // Assignment
        // Copy assignment operator
        constexpr inplace_vector& operator=(const inplace_vector& other)
        noexcept(std::is_nothrow_copy_assignable_v<storage_type>)
        requires std::assignable_from<storage_type&, const storage_type&> = default;

        // Move assignment operator
        constexpr inplace_vector& operator=(inplace_vector&& other)
        noexcept(std::is_nothrow_move_assignable_v<storage_type>)
        requires std::assignable_from<storage_type&, storage_type&&> = default;

        // Element access
        [[nodiscard]] pointer data() noexcept
        requires (!std::is_const_v<value_type>) { // Disabled for const value_type
            return storage_.data();
        }

        [[nodiscard]] const_pointer data() const noexcept {
            return storage_.data();
        }

        [[nodiscard]] constexpr reference front() noexcept
        requires (!std::is_const_v<value_type>) {
            return storage_.data()[0];
        }

        [[nodiscard]] constexpr const_reference front() const noexcept {
            return storage_.data()[0];
        }

        [[nodiscard]] constexpr reference back() noexcept
        requires (!std::is_const_v<value_type>) {
            return storage_.data()[storage_.size() - 1];
        }

        [[nodiscard]] constexpr const_reference back() const noexcept {
            return storage_.data()[storage_.size() - 1];
        }

        // Capacity
        [[nodiscard]] constexpr size_type size() const noexcept {
            return storage_.size();
        }

        [[nodiscard]] constexpr std::size_t ssize() const noexcept {
            return static_cast<std::size_t>(storage_.size());
        }

        [[nodiscard]] constexpr bool empty() const noexcept {
            return size() == 0U;
        }

        [[nodiscard]] static consteval size_type capacity() noexcept {
            return N;
        }

        [[nodiscard]] static consteval size_type max_size() noexcept {
            return N;
        }

        consteval void shrink_to_fit() const noexcept { /* nop */ }


        // Iterators
        [[nodiscard]] iterator begin() noexcept requires (!std::is_const_v<value_type>) {
            return storage_.data();
        }

        [[nodiscard]] const_iterator begin() const noexcept {
            return storage_.data();
        }

        [[nodiscard]] iterator end() noexcept requires (!std::is_const_v<value_type>) {
            return begin() + storage_.size();
        }

        [[nodiscard]] const_iterator end() const noexcept {
            return begin() + storage_.size();
        }

        [[nodiscard]] const_reverse_iterator rbegin() noexcept requires (!std::is_const_v<value_type>) {
            return std::reverse_iterator{ end() };
        }

        [[nodiscard]] const_reverse_iterator rend() noexcept requires (!std::is_const_v<value_type>) {
            return std::reverse_iterator{ begin() };
        }

        [[nodiscard]] const_iterator cbegin() const noexcept {
            return storage_.data();
        }

        [[nodiscard]] const_iterator cend() const noexcept {
            return begin() + storage_.size();
        }

        [[nodiscard]] const_reverse_iterator crbegin() const noexcept {
            return std::reverse_iterator{ cend() };
        }

        [[nodiscard]] const_reverse_iterator crend() const noexcept {
            return std::reverse_iterator{ cbegin() };
        }

        constexpr void clear() noexcept(noexcept(storage_.clear()))
        requires (!std::is_const_v<value_type>) {
            storage_.clear();
        }

    }; // class inplace_vector

} // namespace urlicht

#endif //INPLACE_VECTOR_H
