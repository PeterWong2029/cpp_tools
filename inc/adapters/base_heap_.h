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
#include <vector>

template<typename Container>
concept HeapableContainer =
    std::ranges::range<Container> &&
    requires(Container cont, size_t i, typename Container::value_type val) {
    typename Container::value_type;
    typename Container::size_type;
    typename Container::const_iterator;
    { cont[i] } -> std::convertible_to<typename Container::value_type>;
    { cont.back() } -> std::convertible_to<typename Container::value_type>;
    { cont.front() } -> std::convertible_to<typename Container::value_type>;
    { cont.size() } -> std::convertible_to<size_t>;
    { cont.empty() } -> std::convertible_to<bool>;
    { cont.emplace_back(val) };
    { cont.push_back(val) };
    { cont.pop_back() };
    { cont.clear() };
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
class BaseHeap {
protected:
    using value_type = typename Container::value_type;
    using size_type = typename Container::size_type;

    using const_reference = const value_type&;
    using const_iterator = typename Container::const_iterator;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    Container data_{};
    [[no_unique_address]] Compare compare_{};

    virtual void heapify_up(size_t) = 0;
    virtual void heapify_down(size_t) = 0;

    //The following methods are called after checking all the concepts requirements
    template <bool IsInitialize = true, std::ranges::input_range Rng>
    void insert_range(Rng&& range) {
        iterator_insert<IsInitialize>(std::ranges::begin(range), std::ranges::end(range));
    }

    template <bool IsInitialize = true, std::input_iterator InputIt>
    void iterator_insert(InputIt begin, InputIt end) {
        if constexpr (std::random_access_iterator<InputIt> && requires(size_type i) { data_.reserve(i); }) {
            const auto dist = static_cast<size_type>(end - begin);
            if constexpr (IsInitialize) {
                data_.reserve(dist);
            }
            else {
                data_.reserve(dist + this->data_.size());
            }
        }
        for (auto it = begin; it != end; ++it) {
            data_.emplace_back(std::forward<decltype(*it)>(*it));
        }
    }

    //Constructors
    //Default constructor
    BaseHeap()
    noexcept(std::is_nothrow_default_constructible_v<Container> &&
        std::is_nothrow_default_constructible_v<Compare>)
    requires std::default_initializable<Container> && std::default_initializable<Compare>
        : data_{}, compare_{} {}

    //List-initialization
    BaseHeap(std::initializer_list<value_type> init)
        : BaseHeap(init.begin(), init.end()) {
    }

    template <typename Cmp>
    requires std::constructible_from<Compare, Cmp&&>
    BaseHeap(std::initializer_list<value_type> init, Cmp&& cmp)
        : BaseHeap(init.begin(), init.end(), cmp) {}

    //Constructor with Compare provided
    template <typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> && std::default_initializable<Container>
    explicit(!std::convertible_to<Cmp&&, Compare>)
    BaseHeap(Cmp&& cmp)
        noexcept(std::is_nothrow_constructible_v<Compare, Cmp&&> &&
                 std::is_nothrow_default_constructible_v<Container>)
        : data_{}, compare_{std::forward<Cmp>(cmp)} {
    }

    /*We do not provide container constructors since they are covered by range constructors*/

    //Iterator constructors
    template <std::input_iterator InputIt>
    requires std::default_initializable<Compare> &&
             std::constructible_from<Container, InputIt, InputIt> &&
             std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit BaseHeap(InputIt begin, InputIt end)
        : data_(begin, end), compare_{} {
    }

    template <std::input_iterator InputIt, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
             std::constructible_from<Container, InputIt, InputIt> &&
                std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit BaseHeap(InputIt begin, InputIt end, Cmp&& cmp)
        : data_(begin, end), compare_{std::forward<Cmp>(cmp)} {
    }

    template <std::input_iterator InputIt>
    requires std::default_initializable<Compare> &&
            (!std::constructible_from<Container, InputIt, InputIt>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit BaseHeap(InputIt begin, InputIt end)
        : data_{}, compare_{} {
        iterator_insert(begin, end);
    }

    template <std::input_iterator InputIt, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
            (!std::constructible_from<Container, InputIt, InputIt>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::iter_reference_t<InputIt>>
    explicit BaseHeap(InputIt begin, InputIt end, Cmp&& cmp)
        : data_{}, compare_{std::forward<Cmp>(cmp)} {
        iterator_insert(begin, end);
    }

    //Range Constructors
    template <std::ranges::input_range Rng>
    requires std::default_initializable<Compare> &&
             std::constructible_from<Container, Rng&&> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit BaseHeap(Rng&& range)
        : data_(std::forward<Rng>(range)), compare_{} {
    }

    template <std::ranges::input_range Rng, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
             std::constructible_from<Container, Rng&&> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit BaseHeap(Rng&& range, Cmp&& cmp)
        : data_(std::forward<Rng>(range)), compare_{std::forward<Cmp>(cmp)} {}

    template <std::ranges::input_range Rng>
    requires std::default_initializable<Compare> &&
            (!std::constructible_from<Container, Rng&&>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit BaseHeap(Rng&& range)
        : data_{}, compare_{} {
        insert_range(range);
    }

    template <std::ranges::input_range Rng, typename Cmp>
    requires std::constructible_from<Compare, Cmp&&> &&
            (!std::constructible_from<Container, Rng&&>) &&
             std::default_initializable<Container> &&
             std::constructible_from<value_type, std::ranges::range_reference_t<Rng>>
    explicit BaseHeap(Rng&& range, Cmp&& cmp)
        : data_{}, compare_{std::forward<Cmp>(cmp)} {
        insert_range(range);
    }

    ~BaseHeap() = default;

public:

    /* The following methods are supposed to be universal among all derived classes. */
    //accessors
    [[nodiscard]] const_reference top() const {
        if (empty()) throw std::out_of_range("Heap is empty");
        return data_.front();
    }
    [[nodiscard]] const_reference unsafe_top() const noexcept { return data_[0]; }
    [[nodiscard]] const_iterator begin() const noexcept { return data_.begin(); }
    [[nodiscard]] const_iterator end() const noexcept { return data_.end(); }

    //Capacity
    [[nodiscard]] bool empty() const noexcept { return data_.empty(); }
    [[nodiscard]] size_type size() const noexcept { return data_.size(); }

    void shrink_to_fit() noexcept(noexcept(data_.shrink_to_fit()))
    requires requires { data_.shrink_to_fit(); } {
        data_.shrink_to_fit();
    }

    void reserve(size_type i) noexcept(noexcept(data_.reserve(i)))
    requires requires(size_type n) { data_.reserve(n); } {
        data_.reserve(i);
    }

    /* Derived classes should implement their own versions of, or disable, the following methods */

    //Modifiers
    template <typename Input>
    requires std::constructible_from<T, Input&&>
    void push(Input&& value) {
        data_.emplace_back(std::forward<Input>(value));
        heapify_up(data_.size() - 1);
    }

    template <typename... Args>
    requires std::constructible_from<T, Args&&...>
    void emplace(Args&& ...args) {
        data_.emplace_back(std::forward<Args>(args)...);
        heapify_up(data_.size() - 1);
    }

    template <typename Input>
    requires std::constructible_from<T, Input&&>
    void replace_top(Input&& value) {
        if (empty()) {
            data_.push_back(std::forward<Input>(value));
            return;
        }
        data_[0] = std::forward<Input>(value);
        heapify_down(0);
    }

    template <typename Input>
    requires std::constructible_from<T, Input&&>
    void replace_top_unsafe(Input&& value) {
        data_[0] = std::forward<Input>(value);
        heapify_down(0);
    }

    void pop() {
        if(empty()) {
            throw std::out_of_range("Heap is empty");
        }
        data_[0] = std::move(data_.back());
        data_.pop_back();
        if(!empty()) {
            heapify_down(0);
        }
    }

    void unsafe_pop() noexcept(std::is_nothrow_move_assignable_v<value_type>) {
        data_[0] = std::move(data_.back());
        data_.pop_back();
        if(!empty()) {
            heapify_down(0);
        }
    }

    void clear() noexcept(noexcept(data_.clear())) { data_.clear(); }

    void swap(BaseHeap& rhs)
    noexcept(std::is_nothrow_swappable_v<Container> && std::is_nothrow_swappable_v<Compare>) {
        using std::swap;
        swap(rhs.data_, data_);
        swap(rhs.compare_, compare_);
    }

};
#endif //BASE_HEAP__H
