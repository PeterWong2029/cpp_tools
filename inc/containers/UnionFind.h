//
// Created by Peter on 10/4/2025.
//

#ifndef UNIONFIND_H
#define UNIONFIND_H
#include <concepts>
#include <numeric>
#include <optional>
#include <stdexcept>
#include <functional>
#include <unordered_map>
#include <variant>

/* TODO: Heterogeneous Lookups to avoid implicit type cast*/

template<typename T>
concept Hashable = requires(T a) {
    { std::hash<T>{}(a) } -> std::convertible_to<std::size_t>;
};

template <typename T, bool TrackSize = false>
requires std::copyable<T> && std::equality_comparable<T> && Hashable<T>
class UnionFind {
public:
    using value_type = T;
    using size_type = std::size_t;

private:
    using ParentStorage = std::conditional_t<std::integral<value_type>,
            std::vector<value_type>, std::unordered_map<value_type, value_type>>;

    using RankStorage = std::conditional_t<std::integral<value_type>,
            std::vector<size_type>, std::unordered_map<value_type, size_type>>;

    using SizeStorage = std::conditional_t<std::integral<value_type>,
            std::vector<size_type>, std::unordered_map<value_type, size_type>>;

    mutable ParentStorage parents_;
    mutable RankStorage ranks_;
    mutable std::conditional_t<TrackSize, SizeStorage, std::monostate> sizes_;
    size_type set_count_{0};

    const value_type& get_ensured_parent(const value_type& x) const noexcept {
        value_type root{x}, parent{};
        while (true) {
            if constexpr (std::integral<value_type>) parent = parents_[root];
            else parent = parents_.find(root)->second;

            if (parent == root) break;
            root = parent;
        }
        parent = x;
        while (parent != root) {
            if constexpr (std::integral<value_type>) {
                auto& at = parents_[parent];
                parent = at;
                at = root;
            }
            else {
                auto it = parents_.find(parent);
                parent = std::move(it->second);
                it->second = root;
            }
        }
        return root;
    }

    void unite_ensured_root(const value_type& root_x, const value_type& root_y) noexcept {
        if (root_x == root_y) {
            return;
        }
        value_type rank_x{}, rank_y{};
        if constexpr (std::integral<value_type>) {
            rank_x = ranks_[root_x];
            rank_y = ranks_[root_y];
        }
        else {
            rank_x = ranks_.find(root_x)->second;
            rank_y = ranks_.find(root_y)->second;
        }
        if (rank_x < rank_y) {
            if constexpr (std::integral<value_type>) {
                parents_[root_x] = root_y;
                if constexpr (TrackSize) sizes_[root_y] += sizes_[root_x];
            }
            else {
                parents_.find(root_x)->second = root_y;
                if constexpr (TrackSize) sizes_.find(root_y)->second += sizes_.find(root_x)->second;
            }
        }
        else if (rank_x > rank_y) {
            if constexpr (std::integral<value_type>) {
                parents_[root_y] = root_x;
                if constexpr (TrackSize) sizes_[root_x] += sizes_[root_y];
            }
            else {
                parents_.find(root_y)->second = root_x;
                if constexpr (TrackSize) sizes_.find(root_x)->second += sizes_.find(root_y)->second;
            }
        }
        else {
            if constexpr (std::integral<value_type>) {
                parents_[root_x] = root_y;
                ++ranks_[root_y];
                if constexpr (TrackSize) sizes_[root_y] += sizes_[root_x];
            }
            else {
                parents_.find(root_x)->second = root_y;
                ++ranks_.find(root_y)->second;
                if constexpr (TrackSize) sizes_.find(root_y)->second += sizes_.find(root_x)->second;
            }
        }
        --set_count_;
    }

public:
    // Constructors and Destructor
    UnionFind() requires (!std::integral<value_type>) = default;

    explicit UnionFind(size_type n)
    requires std::integral<value_type>
    : parents_(n), ranks_(n, 1), set_count_(n) {
        std::iota(parents_.begin(), parents_.end(), 0);
        if constexpr (TrackSize) {
            sizes_.assign(n, 1);
        }
    }

    template <typename InputIt>
    requires (!std::integral<value_type>) && std::input_iterator<InputIt>
            && std::same_as<
                std::remove_cvref_t<typename std::iterator_traits<InputIt>::value_type>,
                std::remove_cvref_t<T>>
    UnionFind(InputIt first, InputIt last) {
        for (; first != last; ++first) {
            insert(*first);
        }
    }

    UnionFind(const UnionFind& other) = default;
    UnionFind(UnionFind&& other) = default;
    UnionFind& operator=(const UnionFind& other) = default;
    UnionFind& operator=(UnionFind&& other) = default;
    ~UnionFind() = default;

    // Modifiers - for non-integral types only
    void insert(const value_type& x) requires (!std::integral<value_type>) {
        if (parents_.contains(x)) return;
        parents_[x] = x;
        ranks_[x] = 1;
        if constexpr (TrackSize) sizes_[x] = 1;
        ++set_count_;
    }

    template <std::ranges::range Container>
    requires (!std::integral<value_type>)
            && std::same_as<std::remove_cvref_t<typename Container::value_type>,
                            std::remove_cvref_t<value_type>>
    void insert(const Container& cont) {
        if constexpr (requires(const Container& c) { c.size(); }) {
            const auto new_elem_cnt = static_cast<size_type>(cont.size());
            parents_.reserve(parents_.size() + new_elem_cnt);
            ranks_.reserve(ranks_.size() + new_elem_cnt);
            if constexpr (TrackSize) {
                sizes_.reserve(sizes_.size() + new_elem_cnt);
            }
        }
        for (const auto& x : cont) {
            insert(x);
        }
    }

    template <typename InputIt>
    requires (!std::integral<value_type>)
            && std::input_iterator<InputIt>
            && std::same_as<std::remove_cvref_t<typename std::iterator_traits<InputIt>::value_type>,
                            std::remove_cvref_t<value_type>>
    void insert(InputIt first, InputIt last) {
        if constexpr (std::random_access_iterator<InputIt>) {
            const auto new_elem_cnt = static_cast<size_type>(last - first);
            parents_.reserve(parents_.size() + new_elem_cnt);
            ranks_.reserve(ranks_.size() + new_elem_cnt);
            if constexpr (TrackSize) {
                sizes_.reserve(sizes_.size() + new_elem_cnt);
            }
        }
        for (; first != last; ++first) {
            insert(*first);
        }
    }

    // Modifier for integral types
    void resize(size_type new_size)
    requires std::integral<value_type> {
        const auto old_size = parents_.size();
        // Disjoint set union does not support detaching elements from a set
        if (new_size <= old_size) return;

        parents_.resize(new_size);
        std::iota(parents_.begin() + old_size, parents_.begin() + new_size, old_size);

        ranks_.resize(new_size);
        std::fill(ranks_.begin() + old_size, ranks_.begin() + new_size, 1);

        if constexpr (TrackSize) {
            sizes_.resize(new_size);
            std::fill(sizes_.begin() + old_size, sizes_.begin() + new_size, 1);
        }
        set_count_ += new_size - old_size;
    }

    // Core methods
    [[nodiscard]] const value_type& find_ensured(const value_type& x) {
        if constexpr (std::integral<value_type>) {
            if (x >= parents_.size()) this->resize(x + 1);
        }
        else {
            if (!contains(x)) insert(x);
        }
        return get_ensured_parent(x);
    }

    [[nodiscard]] const value_type& find_existing(const value_type& x) const {
        if constexpr (std::integral<value_type>) {
            if (x >= parents_.size()) throw std::out_of_range("UnionFind index out of range");
        }
        else {
            if (!contains(x)) throw std::out_of_range("UnionFind key not found");
        }
        return get_ensured_parent(x);
    }

    [[nodiscard]] const value_type& find_existing_unsafe(const value_type& x) const noexcept {
        return get_ensured_parent(x);
    }

    [[nodiscard]] std::optional<const value_type&> try_find(const value_type& x) const noexcept {
        if constexpr (std::integral<value_type>) {
            if (x >= parents_.size()) return std::nullopt;
        }
        else {
            if (!parents_.contains(x)) return std::nullopt;
        }
        return get_ensured_parent(x);
    }

    void unite_new(const value_type& x, const value_type& y) {
        const auto root_x = find_ensured(x);
        const auto root_y = find_ensured(y);
        unite_ensured_root(root_x, root_y);
    }

    void unite_existing(const value_type& x, const value_type& y)  {
        const auto root_x = find_existing(x);
        const auto root_y = find_existing(y);
        unite_ensured_root(root_x, root_y);
    }

    void unite_existing_unsafe(const value_type& x, const value_type& y) {
        const auto root_x = find_existing_unsafe(x);
        const auto root_y = find_existing_unsafe(y);
        unite_ensured_root(root_x, root_y);
    }

    bool try_unite(const value_type& x, const value_type& y) {
        const auto root_x = try_find(x);
        const auto root_y = try_find(y);
        if (root_x == std::nullopt || root_y == std::nullopt) {
            return false;
        }
        unite_ensured_root(root_x.value(), root_y.value());
        return true;
    }

    // Query Methods
    [[nodiscard]] bool same_set(const value_type& x, const value_type& y) const noexcept {
        const auto root_x = try_find(x);
        const auto root_y = try_find(y);
        return root_x != std::nullopt && root_x == root_y;
    }

    [[nodiscard]] size_type size() const noexcept {
        return parents_.size();
    }

    [[nodiscard]] size_type set_rank(const value_type& x) const noexcept {
        auto root = try_find(x);
        if (root == std::nullopt) {
            return 0U;
        }
        if constexpr (!std::integral<value_type>) {
            return ranks_.find(root.value())->second;
        }
        else return ranks_[root.value()];
    }

    [[nodiscard]] size_type set_size(const value_type& x) const noexcept
    requires TrackSize {
        auto root = try_find(x);
        if (root == std::nullopt) {
            return 0U;
        }
        if constexpr (!std::integral<value_type>) {
            return sizes_.find(root.value())->second;
        }
        else return sizes_[root.value()];
    }

    [[nodiscard]] bool empty() const noexcept {
        return parents_.empty();
    }

    [[nodiscard]] size_type set_count() const noexcept {
        return set_count_;
    }

    [[nodiscard]] bool contains(const value_type& x) const noexcept
    requires (!std::integral<value_type>) {
        return parents_.contains(x);
    }

    void clear() noexcept {
        parents_.clear();
        ranks_.clear();
        if constexpr (TrackSize) {
            sizes_.clear();
        }
        set_count_ = 0U;
    }
};

#endif //UNIONFIND_H

