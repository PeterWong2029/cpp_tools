//
// Created by Peter on 9/22/2025.
//

#ifndef BASE_HEAP__H
#define BASE_HEAP__H

#include <vector>
#include <concepts>
#include <functional>
#include <concepts>
#include <ranges>

template<typename Container>
concept HeapableContainer = // Examples: std::vector, std::deque, std::inplace_vector
    std::ranges::range<Container> &&
    requires(Container cont, size_t i, typename Container::value_type val) {
    typename Container::value_type;
    typename Container::size_type;
    typename Container::const_iterator;
    { cont[i] } -> std::convertible_to<typename Container::value_type>;
    { cont.back() } -> std::convertible_to<typename Container::value_type>;
    { cont.front() } -> std::convertible_to<typename Container::value_type>;
    { cont.size() } -> std::convertible_to<std::size_t>;
    { cont.capacity() } -> std::convertible_to<std::size_t>;
    { cont.empty() } -> std::convertible_to<bool>;
    { cont.emplace_back(val) };
    { cont.push_back(val) };
    { cont.pop_back() };
    { cont.clear() };
    };

template<typename Container>
concept Reservable = requires(Container cont)
{
    { cont.reserve() };
};

template<typename Compare, typename Container>
concept HeapFunctor =
    HeapableContainer<Container> &&
    requires(Compare comp, typename Container::value_type val)
{
    { comp(val, val) } -> std::same_as<bool>;
};

template <
    typename T,
    HeapableContainer Container = std::vector<T>,
    HeapFunctor<Container> Compare = std::less<T>>
requires std::same_as<T, typename Container::value_type> && std::is_object_v<T>
class base_heap {
public:
    using value_type = typename Container::value_type;
    using size_type = typename Container::size_type;
    using difference_type = std::ptrdiff_t;
    using container_type = Container;
    using value_compare = Compare;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using iterator = typename Container::const_iterator;
    using const_iterator = typename Container::const_iterator;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
protected:
    Container data_{};
    [[no_unique_address]] Compare compare_{};

    //The following methods are called after checking all the concepts requirements
    template <std::ranges::input_range Rng>
    constexpr void insert_range(Rng&& range) {
        iterator_insert(std::ranges::begin(range), std::ranges::end(range));
    }

    template <std::input_iterator InputIt>
    constexpr void iterator_insert(InputIt begin, InputIt end) {
        if constexpr (std::random_access_iterator<InputIt> && Reservable<Container>) {
            const auto dist = static_cast<size_type>(end - begin);
            this->data_.reserve(dist + this->data_.size());
        }
        for (auto it = begin; it != end; ++it) {
            data_.emplace_back(std::forward<decltype(*it)>(*it));
        }
    }

    //Constructors
    //Default constructor
    constexpr base_heap()
    noexcept(std::is_nothrow_default_constructible_v<Container> &&
        std::is_nothrow_default_constructible_v<Compare>)
    requires std::default_initializable<Container> && std::default_initializable<Compare>
        : data_{}, compare_{} {}

    //Constructor with Compare provided
    template <typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> && std::default_initializable<Container>
    explicit(!std::convertible_to<Cmp&&, Compare>)
    constexpr base_heap(Cmp&& cmp)
        noexcept(std::is_nothrow_constructible_v<Compare, Cmp&&> &&
                 std::is_nothrow_default_constructible_v<Container>)
        : data_{}, compare_{std::forward<Cmp>(cmp)} {
    }

    /* We do not provide container constructors since they are covered by range constructors */

    //Iterator constructors
    template <std::input_iterator InputIt>
    requires std::default_initializable<Compare> &&
             std::constructible_from<Container, InputIt, InputIt> &&
             std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit constexpr base_heap(InputIt begin, InputIt end)
        : data_(begin, end), compare_{} {
    }

    template <std::input_iterator InputIt, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
             std::constructible_from<Container, InputIt, InputIt> &&
                std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit constexpr base_heap(InputIt begin, InputIt end, Cmp&& cmp)
        : data_(begin, end), compare_{std::forward<Cmp>(cmp)} {
    }

    template <std::input_iterator InputIt>
    requires std::default_initializable<Compare> &&
            (!std::constructible_from<Container, InputIt, InputIt>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit constexpr base_heap(InputIt begin, InputIt end)
        : data_{}, compare_{} {
        iterator_insert(begin, end);
    }

    template <std::input_iterator InputIt, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
            (!std::constructible_from<Container, InputIt, InputIt>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit constexpr base_heap(InputIt begin, InputIt end, Cmp&& cmp)
        : data_{}, compare_{std::forward<Cmp>(cmp)} {
        iterator_insert(begin, end);
    }

    //Range Constructors
    template <std::ranges::input_range Rng>
    requires std::default_initializable<Compare> &&
             std::constructible_from<Container, Rng&&> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit constexpr base_heap(Rng&& range)
        : data_(std::forward<Rng>(range)), compare_{} {
    }

    template <std::ranges::input_range Rng, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
             std::constructible_from<Container, Rng&&> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit constexpr base_heap(Rng&& range, Cmp&& cmp)
        : data_(std::forward<Rng>(range)), compare_{std::forward<Cmp>(cmp)} {
    }

    template <std::ranges::input_range Rng>
    requires std::default_initializable<Compare> &&
            (!std::constructible_from<Container, Rng&&>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit constexpr base_heap(Rng&& range)
        : data_{}, compare_{} {
        insert_range(range);
    }

    template <std::ranges::input_range Rng, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
            (!std::constructible_from<Container, Rng&&>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit constexpr base_heap(Rng&& range, Cmp&& cmp)
        : data_{}, compare_{std::forward<Cmp>(cmp)} {
        insert_range(range);
    }

    constexpr ~base_heap() = default;

public:
    /* The following methods are supposed to be universal among all derived classes. */
    //accessors
    [[nodiscard]] constexpr const_reference top() const {
        if (empty()) throw std::out_of_range("Heap is empty");
        return data_.front();
    }

    [[nodiscard]] constexpr const_reference unchecked_top() const noexcept {
        return data_[0];
    }

    [[nodiscard]] constexpr const_reference back() const noexcept(noexcept(data_.back())) {
        return data_.back();
    }

    //Modifier
    constexpr void pop_back() noexcept(noexcept(data_.back())) {
        data_.pop_back();
    }

    // Iterators
    // Note that iteration over a heap is in general meaningless
    [[nodiscard]] constexpr const_iterator begin() const noexcept { return data_.begin(); }
    [[nodiscard]] constexpr const_iterator end() const noexcept { return data_.end(); }
    [[nodiscard]] constexpr const_reverse_iterator rbegin() const noexcept { return std::reverse_iterator{ begin() }; }
    [[nodiscard]] constexpr const_reverse_iterator rend() const noexcept { return std::reverse_iterator{ end() }; }
    [[nodiscard]] constexpr const_iterator cbegin() const noexcept { return data_.cbegin(); }
    [[nodiscard]] constexpr const_iterator cend() const noexcept { return data_.cend(); }
    [[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept { return std::reverse_iterator{ cend() }; }
    [[nodiscard]] constexpr const_reverse_iterator crend() const noexcept { return std::reverse_iterator{ cbegin() }; }

    //Capacity
    [[nodiscard]] constexpr bool empty() const noexcept(noexcept(data_.empty())) {
        return data_.empty();
    }

    [[nodiscard]] constexpr size_type size() const noexcept(noexcept(data_.size())) {
        return data_.size();
    }

    [[nodiscard]] constexpr size_type max_size() const noexcept(noexcept(data_.max_size()))
    requires requires { data_.max_size(); } {
        return data_.max_size();
    }

    [[nodiscard]] constexpr size_type capacity() const noexcept(noexcept(data_.capacity()))
    requires requires { data_.capacity(); } {
        return data_.capacity();
    }

    constexpr void shrink_to_fit() noexcept(noexcept(data_.shrink_to_fit()))
    requires requires { data_.shrink_to_fit(); } {
        data_.shrink_to_fit();
    }

    constexpr void reserve(size_type i)
    requires requires(size_type n) { data_.reserve(n); } {
        data_.reserve(i);
    }

};
#endif //BASE_HEAP__H

