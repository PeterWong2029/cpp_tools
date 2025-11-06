//
// Created by Peter on 9/21/2025.
//

#ifndef STATICPODVECTOR_H
#define STATICPODVECTOR_H
#include <cassert>
#include <functional>
#include <cstddef>
#include <stdexcept>
#include <iterator>
#include <type_traits>
#include <cstring>



template <typename T, size_t N>
    requires std::is_trivial_v<T>
class static_pod_vector {
    private:
    alignas(sizeof(T)) std::byte data_[sizeof(T) * N]{};
    size_t size_{0};

    T* ptr_at(const size_t pos) noexcept {
        return reinterpret_cast<T*>(data_) + pos;
    }

    const T* ptr_at(const size_t pos) const noexcept {
        return reinterpret_cast<const T*>(data_) + pos;
    }

    public:
    using value_type = T;
    using size_type = size_t;
    using difference_type = std::ptrdiff_t;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    static_pod_vector() noexcept = default;

    ~static_pod_vector() noexcept = default;

    static_pod_vector(const static_pod_vector& other) noexcept {
        std::memcpy(data_, other.data_, other.size_ * sizeof(T));
        size_ = other.size_;
    }

    static_pod_vector(static_pod_vector&& other) noexcept {
        std::memcpy(data_, other.data_, other.size_ * sizeof(T));
        size_ = other.size_;
        other.size_ = 0;
    }

    template <typename InputIt>
        requires std::random_access_iterator<InputIt> &&
                std::same_as<
                    std::remove_cvref_t<T>,
                    std::remove_cvref_t<typename std::iterator_traits<InputIt>::value_type>>
    static_pod_vector(const InputIt begin, const InputIt end) noexcept {
        const auto cnt = std::min(static_cast<size_t>(end - begin), N);
        std::memcpy(data_, std::to_address(begin), cnt * sizeof(T));
        size_ = cnt;
    }

    static_pod_vector& operator=(const static_pod_vector& other) noexcept {
        if (this != &other) {
            std::memcpy(data_, other.data_, other.size_ * sizeof(T));
            size_ = other.size_;
        }
        return *this;
    }

    static_pod_vector& operator=(static_pod_vector&& other) noexcept {
        if (this == &other) {
            std::memcpy(data_, other.data_, other.size_ * sizeof(T));
            size_ = other.size_;
            other.size_ = 0;
        }
        return *this;
    }

    reference operator[](const size_t pos) noexcept { return *ptr_at(pos); }
    const_reference operator[](const size_t pos) const noexcept { return *ptr_at(pos); }

    reference at(size_t pos) {
        if (pos >= size_) throw std::out_of_range("static_vector");
        return *ptr_at(pos);
    }

    const_reference at(size_t pos) const {
        if (pos >= size_) throw std::out_of_range("static_vector");
        return *ptr_at(pos);
    }

    reference front() noexcept { return *ptr_at(0); }
    const_reference front() const noexcept { return *ptr_at(0); }
    reference back() noexcept { return *ptr_at(size_ - 1); }
    const_reference back() const noexcept { return *ptr_at(size_ - 1); }
    pointer data() noexcept { return ptr_at(0); }
    const_pointer data() const noexcept { return ptr_at(0); }

    iterator begin() noexcept { return ptr_at(0); }
    const_iterator begin() const noexcept { return ptr_at(0); }
    const_iterator cbegin() const noexcept { return ptr_at(0); }
    iterator end() noexcept { return ptr_at(size_); }
    const_iterator end() const noexcept { return ptr_at(size_); }
    const_iterator cend() const noexcept { return ptr_at(size_); }

    reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }
    const_reverse_iterator crbegin() const noexcept { return const_reverse_iterator(end()); }
    reverse_iterator rend() noexcept { return reverse_iterator(begin()); }
    const_reverse_iterator crend() const noexcept { return const_reverse_iterator(begin()); }

    [[nodiscard]] bool empty() const noexcept { return size_ == 0; }
    [[nodiscard]] size_t size() const noexcept { return size_; }
    static constexpr size_t capacity() noexcept { return N; }

    template <typename... Args>
    void emplace_back(Args&&... args) noexcept {
        new (ptr_at(size_)) T(std::forward<Args>(args)...);
        ++size_;
    }

    void push_back(const T& value) noexcept {
        new (ptr_at(size_)) T(value);
        ++size_;
    }

    void push_back(T&& value) noexcept {
        new (ptr_at(size_)) T(std::move(value));
        ++size_;
    }

    void safe_push_back(const T& value) {
        if (size_ >= N) throw std::out_of_range("static_vector");
        new (ptr_at(size_)) T(value);
        ++size_;
    }

    void safe_push_back(T&& value) {
        if (size_ >= N) throw std::out_of_range("static_vector");
        new (ptr_at(size_)) T(value);
        ++size_;
    }

    void pop_back() noexcept { if (size_) --size_; }

    void clear() noexcept { size_ = 0; }

    void reserve(size_t) noexcept {}

};
#endif //STATICPODVECTOR_H
