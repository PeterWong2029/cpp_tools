#ifndef TRIE_H
#define TRIE_H

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <string_view>
#include <vector>
#include <stdexcept>

template <typename Container>
concept StringSequence =
    std::ranges::range<Container> &&
    (std::convertible_to<std::ranges::range_value_t<Container>, std::string>
    || std::same_as<std::remove_const_t<std::ranges::range_value_t<Container>>, std::string_view>)
    && requires(Container c, std::string str) {
        c.push_back(str);
        c.pop_back();
    };


template <typename T>
requires std::same_as<std::remove_const_t<T>, std::string>
        || StringSequence<std::remove_const_t<T>>
// Generalized version for string sequences (e.g. std::vector<std::string>)
class Trie {
public:
    using value_type = T;
    using edge_type = std::ranges::range_value_t<T>;

private:
    struct TrieNode {
        bool end{false};
        std::unordered_map<edge_type, int> next;
    };

    std::vector<TrieNode> nodes;
public:
    // --- CONSTRUCTORS ---
    explicit Trie(const std::size_t n = 1000) {
        nodes.reserve(n);
        nodes.emplace_back();
        nodes.emplace_back();
    }

    template <typename Container>
    requires std::ranges::range<Container>
          && std::same_as<std::ranges::range_value_t<Container>, value_type>
    explicit Trie(const Container& cont)
        : Trie([](const Container& c) {
            if constexpr (requires { c.size(); })
                return static_cast<std::size_t>(c.size() * 5);
            else
                return static_cast<std::size_t>(1000);
        }(cont)) {
        for (const auto& sequence : cont) {
            insert(sequence);
        }
    }

    template <typename InputIt>
    requires std::input_iterator<InputIt>
          && std::same_as<
              std::remove_const_t<typename std::iterator_traits<InputIt>::value_type>,
              std::remove_const_t<value_type>>
    Trie(InputIt first, InputIt last)
        : Trie([](InputIt f, InputIt l) {
            if constexpr (std::random_access_iterator<InputIt>)
                return static_cast<size_t>(l - f) * 5;
            else
                return static_cast<size_t>(1000);
        }(first, last)) {
        for (; first != last; ++first) {
            insert(*first);
        }
    }

    ~Trie() = default;
    Trie(const Trie&) = default;
    Trie(Trie&&) = default;
    Trie& operator=(const Trie&) = default;
    Trie& operator=(Trie&&) = default;

    [[nodiscard]] static constexpr int get_root_idx() { return 1; }

    [[nodiscard]] const TrieNode& operator[](const int node_idx) const noexcept {
        return nodes[node_idx];
    }

    // --- CORE METHODS ---

    // For performance, implicit conversion from input value to edge_type is disallowed
    void insert(const value_type& sequence) {
        int node_idx = get_root_idx();
        for (const auto& edge : sequence) {
            auto& next_map = nodes[node_idx].next;
            auto it = next_map.find(edge);

            if (it == next_map.end()) {
                const int new_node_idx = nodes.size();
                nodes.emplace_back();
                next_map[edge] = new_node_idx;
                node_idx = new_node_idx;
            } else {
                node_idx = it->second;
            }
        }
        nodes[node_idx].end = true;
    }

    template <typename InputIt>
    requires std::input_iterator<InputIt> &&
        std::same_as<
            std::remove_const<typename std::iterator_traits<InputIt>::value_type>,
            std::remove_const<value_type>>
    void insert_range(InputIt first, InputIt last) {
        for (; first != last; ++first) {
            insert(*first);
        }
    }

    [[nodiscard]] bool exact_match(const value_type& sequence) const noexcept {
        int node_idx = get_root_idx();
        for (const auto& edge : sequence) {
            const auto& next_map = nodes[node_idx].next;
            auto it = next_map.find(edge);
            if (it == next_map.end()) {
                return false;
            }
            node_idx = it->second;
        }
        return nodes[node_idx].end;
    }

    [[nodiscard]] bool start_with(const value_type& prefix_sequence) const noexcept {
        int node_idx = get_root_idx();
        for (const auto& edge : prefix_sequence) {
            const auto& next_map = nodes[node_idx].next;
            auto it = next_map.find(edge);
            if (it == next_map.end()) {
                return false;
            }
            node_idx = it->second;
        }
        return true;
    }

    [[nodiscard]] std::vector<value_type> autocomplete(const value_type& prefix_sequence) const {
        std::vector<value_type> results;
        int node_idx = get_root_idx();

        for (const auto& edge : prefix_sequence) {
            const auto& next_map = nodes[node_idx].next;
            auto it = next_map.find(edge);
            if (it == next_map.end()) {
                return {};
            }
            node_idx = it->second;
        }

        std::function<void(int, value_type&)> dfs =
            [&](int curr_idx, value_type& path) {
            if (nodes[curr_idx].end) {
                results.push_back(path);
            }

            for (const auto& [edge, next_idx] : nodes[curr_idx].next) {
                path.push_back(edge);
                dfs(next_idx, path);
                path.pop_back();
            }
        };

        value_type path = prefix_sequence;
        dfs(node_idx, path);
        return results;
    }

    void clear() noexcept {
        nodes.clear();
    }
};


template<>
// Specialized version for std::string, where std::array is used to
// store the index of the next TrieNode for max performance.
class Trie<std::string> {
    using value_type = std::string_view;
    private:
    struct TrieNode {
        bool end{false};
        std::array<int, 26> next{}; // set size to 128 to support all ASCII characters
    };
    std::vector<TrieNode> nodes;

    public:
    explicit Trie(const std::size_t n = 1000) {
        nodes.reserve(n);
        nodes.emplace_back();
        nodes.emplace_back();
    }

    template <typename Container>
    requires std::ranges::range<Container>
          && std::convertible_to<std::remove_const_t<typename Container::value_type>, value_type>
    explicit Trie(const Container& cont)
        : Trie([](const Container& c) {
            if constexpr (requires { c.size(); })
                return static_cast<std::size_t>(c.size() * 5);
            else
                return static_cast<std::size_t>(1000);
        }(cont)) {
        for (const auto& it : cont) {
            insert(it);
        }
    }

    template <typename InputIt>
    requires std::input_iterator<InputIt>
          && std::convertible_to<typename InputIt::value_type, value_type>
    Trie(InputIt first, InputIt last)
        : Trie([](InputIt f, InputIt l) {
            if constexpr (std::random_access_iterator<InputIt>)
                return static_cast<size_t>((l - f) * 5);
            else
                return static_cast<size_t>(1000);
        }(first, last)) {
        for (; first != last; ++first) {
            insert(*first);
        }
    }

    ~Trie() = default;

    Trie(const Trie&) = default;
    Trie(Trie&&) = default;
    Trie& operator=(const Trie&) = default;
    Trie& operator=(Trie&&) = default;

    [[nodiscard]] static constexpr int get_root_idx() { return 1; }

    [[nodiscard]] const TrieNode& at(const int node_idx) const {
        if (node_idx >= nodes.size()) {
          throw std::out_of_range("Trie node index out of range");
        }
        return nodes[node_idx];
    }

    [[nodiscard]] const TrieNode& operator[] (const int node_idx) const noexcept {
        return nodes[node_idx];
    }

    void insert(const std::string_view word) {
        int node_idx = get_root_idx();
        for (const char c : word) {
            const int char_idx = c - 'a';
            int& next_idx = nodes[node_idx].next[char_idx];
            if (!next_idx) {
                next_idx = nodes.size();
                nodes.emplace_back();
            }
            node_idx = next_idx;
        }
        nodes[node_idx].end = true;
    }

    template <typename InputIt>
    requires std::input_iterator<InputIt> &&
        std::convertible_to<typename InputIt::value_type, value_type>
    void insert_range(InputIt first, InputIt last) {
        for (; first != last; ++first) {
            insert(*first);
        }
    }

    [[nodiscard]] bool exact_match(const std::string_view word) const noexcept {
        int node_idx = get_root_idx();
        for(const char c : word){
            const int char_idx = c - 'a';
            const int& next_idx = nodes[node_idx].next[char_idx];
            if(!next_idx) {
                return false;
            }
            node_idx = next_idx;
        }
        return nodes[node_idx].end;
    }

    [[nodiscard]] bool start_with(const std::string_view prefix) const noexcept {
        int node_idx = get_root_idx();
        for(const char c : prefix){
            const int char_idx = c - 'a';
            const int& next_idx = nodes[node_idx].next[char_idx];
            if(!next_idx) {
                return false;
            }
            node_idx = next_idx;
        }
        return true;
    }

    [[nodiscard]] bool contains_prefix_of(const std::string_view query) const noexcept {
        int node_idx = get_root_idx();
        for (const char c : query) {
            const int char_idx = c - 'a';
            const int& next_idx = nodes[node_idx].next[char_idx];
            if(!next_idx) {
                return false;
            }
            node_idx = next_idx;
            if (nodes[node_idx].end) {
                return true;
            }
        }
        return false;
    }

    [[nodiscard]] std::string find_prefix_of(const std::string_view query) const noexcept {
        std::string prefix{};
        int node_idx = get_root_idx();
        for (const char c : query) {
            const int char_idx = c - 'a';
            const int& next_idx = nodes[node_idx].next[char_idx];
            if (!next_idx) {
                return {};
            }
            prefix += c;
            node_idx = next_idx;
            if (nodes[node_idx].end) {
                return prefix;
            }
        }
        return {};
    }

    [[nodiscard]] std::vector<std::string> autocomplete(const std::string_view prefix) const {
        std::vector<std::string> results;
        int node_idx = get_root_idx();

        for (const char c : prefix) {
            const int char_idx = c - 'a';
            const int& next_idx = nodes[node_idx].next[char_idx];
            if (!next_idx) {
                return {};
            }
            node_idx = next_idx;
        }

        std::function<void(int, std::string&)> dfs =
                [&](const int curr_idx, std::string& path) {
            if (nodes[curr_idx].end) {
                results.push_back(path);
            }
            for (int i = 0; i < 26; ++i) {
                if (const int& next_idx = nodes[curr_idx].next[i]) {
                    path += static_cast<char>('a' + i);
                    dfs(next_idx, path);
                    path.pop_back();
                }
            }
        };

        std::string path{prefix};
        dfs(node_idx, path);
        return results;
    }

    void clear() noexcept {
        nodes.clear();
    }
};


#endif // TRIE_H