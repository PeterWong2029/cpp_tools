#ifndef DATA_GENERATOR_H
#define DATA_GENERATOR_H

#include <vector>
#include <string>
#include <random>
#include <algorithm>
#include <numeric>
#include <any>
#include <ranges>


enum class DataOrder {
    UNIFORM,
    ASCENDING,
    DESCENDING,
    SPARSE_RANDOM,  // Wide range of values
    NORMAL_RANDOM,  // Normal distributed random values
    COMPACT_RANDOM, // Small range of values (many duplicates)
    PREHEAPIFIED
};

enum class DataType {
    INT,
    DOUBLE,
    STRING,
    VECTOR
};

enum class N {
    _1E3,
    _1E4,
    _1E5,
    _1E6,
    _1E7,
};


class data_generator { // Generates data streams for benchmarking

    static std::mt19937& get_generator() {
        static std::random_device rd;
        static std::mt19937 gen(rd());
        return gen;
    }

    static std::vector<int> create_int(const std::size_t n, const DataOrder order) {
        std::vector<int> result(n);
        auto& gen = get_generator();

        switch (order) {
            case DataOrder::UNIFORM: {
                std::ranges::fill(result, 0);
                break;
            }
            case DataOrder::ASCENDING: {
                std::iota(result.begin(), result.end(), 0);
                break;
            }
            case DataOrder::DESCENDING: {
                std::iota(result.begin(), result.end(), 0);
                std::ranges::reverse(result);
                break;
            }
            case DataOrder::SPARSE_RANDOM: {
                std::uniform_int_distribution dist(std::numeric_limits<int>::min(),
                    std::numeric_limits<int>::max());
                std::ranges::generate(result, [&] { return dist(gen); });
                break;
            }
            case DataOrder::COMPACT_RANDOM: {
                std::uniform_int_distribution dist(0, 100);
                std::ranges::generate(result, [&] { return dist(gen); });
                break;
            }
            case DataOrder::PREHEAPIFIED: {
                std::uniform_int_distribution dist(0, 100000);
                std::ranges::generate(result, [&] { return dist(gen); });
                std::ranges::make_heap(result);
                break;
            }
            default: ;
        }
        return result;
    }

    static std::vector<double> create_double(const std::size_t n, const DataOrder order) {
        std::vector<double> result(n);
        auto& gen = get_generator();

        switch (order) {
            case DataOrder::UNIFORM: {
                std::ranges::fill(result, 0.0);
                break;
            }
            case DataOrder::ASCENDING: {
                std::iota(result.begin(), result.end(), 0.0);
                break;
            }
            case DataOrder::DESCENDING: {
                std::iota(result.begin(), result.end(), 0.0);
                std::ranges::reverse(result);
                break;
            }
            case DataOrder::SPARSE_RANDOM: {
                std::uniform_real_distribution dist(-1e9, 1e9);
                std::ranges::generate(result, [&] { return dist(gen); });
                break;
            }
            case DataOrder::COMPACT_RANDOM: {
                std::uniform_real_distribution dist(0.0, 10.0);
                std::ranges::generate(result, [&] { return dist(gen); });
                break;
            }
            case DataOrder::PREHEAPIFIED: {
                std::uniform_real_distribution dist(0.0, 10000.0);
                std::ranges::generate(result, [&] { return dist(gen); });
                std::ranges::make_heap(result);
                break;
            }
            default:
        }
        return result;
    }

    static std::vector<std::string> create_strings(const std::size_t n, const DataOrder order) {
        std::vector<std::string> result(n);
        auto& gen = get_generator();

        auto create_string = [&](const std::size_t len = 10) {
            static constexpr char ascii_char[] =
                "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                "abcdefghijklmnopqrstuvwxyz"
                "~!@#$%^&*()_+{}|:\"<>?`-=[]\\;',./";
            std::string s;
            s.reserve(len);
            std::uniform_int_distribution<> dist(0, sizeof(ascii_char) - 2);
            for (int i = 0; i < len; ++i) {
                s += ascii_char[dist(gen)];
            }
            return s;
        };

        switch (order) {
            case DataOrder::UNIFORM: {
                std::ranges::fill(result, "ABCDE");
                break;
            }
            case DataOrder::ASCENDING: {
                std::ranges::generate(result, [&] { return create_string(); });
                std::ranges::sort(result);
                break;
            }
            case DataOrder::DESCENDING: {
                std::ranges::generate(result, [&] { return create_string(); });
                std::ranges::sort(result, std::greater{});
                break;
            }
            case DataOrder::SPARSE_RANDOM: {
                // Reduces duplications by creating long strings
                std::ranges::generate(result, [&] { return create_string(75); });
                break;
            }
            case DataOrder::COMPACT_RANDOM: {
                std::vector<std::string> candidates(20);
                std::ranges::generate(candidates, [&] { return create_string(5); });
                std::uniform_int_distribution<size_t> pool_dist(0, candidates.size() - 1);
                std::ranges::generate(result, [&] { return candidates[pool_dist(gen)]; });
                break;
            }
            case DataOrder::PREHEAPIFIED: {
                std::ranges::generate(result, [&] { return create_string(); });
                std::ranges::make_heap(result);
                break;
            }
            default:
        }
        return result;
    }

    static std::vector<std::vector<int>> create_vecs(const std::size_t n, const DataOrder order) {
        std::vector<std::vector<int>> result(n);
        auto& gen = get_generator();

        auto create_vec = [&](const std::size_t len = 10) {
            std::vector<int> v(10);
            std::uniform_int_distribution dist(0, 100);
            std::ranges::generate(v, [&] { return dist(gen); });
            return v;
        };

        switch (order) {
            case DataOrder::UNIFORM: {
                std::ranges::fill(result, std::vector(10, -1));
                break;
            }
            case DataOrder::ASCENDING: {
                std::ranges::generate(result, create_vec);
                std::ranges::sort(result);
                break;
            }
            case DataOrder::DESCENDING: {
                std::ranges::generate(result, create_vec);
                std::ranges::sort(result, std::greater{});
                break;
            }
            case DataOrder::PREHEAPIFIED: {
                std::ranges::generate(result, create_vec);
                std::ranges::make_heap(result);
                break;
            }
            case DataOrder::SPARSE_RANDOM: {
                std::ranges::generate(result, [&] { return create_vec(100); });
                break;
            }
            case DataOrder::COMPACT_RANDOM: {
                std::vector<std::vector<int>> candidates(20);
                std::ranges::generate(candidates, [&] { return create_vec(); });
                std::uniform_int_distribution<size_t> pool_dist(0, candidates.size() - 1);
                std::ranges::generate(result, [&] { return candidates[pool_dist(gen)]; });
                break;
            }
            default:
        }
        return result;
    }

    static constexpr std::size_t to_size(const N n) {
        switch (n) {
            case N::_1E3: return 1000;
            case N::_1E4: return 10000;
            case N::_1E5: return 100000;
            case N::_1E6: return 1000000;
            case N::_1E7: return 10000000;
            default: return 0;
        }
    }

public:
    static std::any create_data(const DataType type = DataType::INT,
                                const N n = N::_1E5,
                                const DataOrder order = DataOrder::ASCENDING){
        const std::size_t size { to_size(n) };
        switch (type) {
            case DataType::INT: return create_int(size, order);
            case DataType::DOUBLE: return create_double(size, order);
            case DataType::STRING: return create_strings(size, order);
            case DataType::VECTOR: return create_vecs(size, order);
            default: return {};
        }
    }
};

