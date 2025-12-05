#ifndef COMPARE_H
#define COMPARE_H
#include <concepts>
#include <functional>
#include "concepts_uility.h"

namespace urlicht::compare {
    template <typename T, typename U = T>
    concept less_comparable =
        // Requires that the comparison function takes const lvalue reference only
        // To ensure that the instances are not modified
        requires(const std::remove_reference_t<T>& lhs, const std::remove_reference_t<U>& rhs)
        {
            { lhs < rhs } -> std::convertible_to<bool>;
        }
        || (concepts::decays_to_ptr<T> && concepts::decays_to_ptr<U>);

    namespace detail {
        template <typename T, typename U = T>
        requires less_comparable<T, U>
        struct total_order_less {
            using PTy = std::remove_reference_t<T>;
            using PUy = std::remove_reference_t<U>;

            static constexpr bool is_nothrow() noexcept {
                if constexpr (concepts::decays_to_ptr<T> && concepts::decays_to_ptr<U>)
                    return true;
                else
                    return noexcept(std::declval<PTy>() < std::declval<PUy>());
            }

            [[nodiscard]]
            constexpr bool operator()(const PTy& lhs, const PUy& rhs) const noexcept(is_nothrow()) {
                if constexpr (concepts::decays_to_ptr<T> && concepts::decays_to_ptr<U>) {
                    return std::bit_cast<std::uintptr_t>(lhs) < std::bit_cast<std::uintptr_t>(rhs);
                }
                else
                    return lhs < rhs;
            }
        };
    }

    template <typename T = void, typename U = T>
    struct less {
        using PTy = std::remove_reference_t<T>;
        using PUy = std::remove_reference_t<U>;

        [[nodiscard]]
        constexpr bool operator()(const PTy& lhs, const PUy& rhs) const
        noexcept(noexcept(detail::total_order_less<T, U>{}(lhs, rhs)))
        requires less_comparable<T, U> {
            return detail::total_order_less<T, U>{}(lhs, rhs);
        }

        [[nodiscard]]
        constexpr bool operator()(const PUy& lhs, const PTy& rhs) const
        noexcept(noexcept(detail::total_order_less<U, T>{}(lhs, rhs)))
        requires (!std::same_as<PTy, PUy>) && less_comparable<U, T> {
            return detail::total_order_less<U, T>{}(lhs, rhs);
        }
    };

    template <>
    struct less<void> {
        using is_transparent = void;

        template <typename T, typename U>
        requires less_comparable<T, U>
        [[nodiscard]]
        constexpr bool operator()(const T& lhs, const U& rhs) const
        noexcept(noexcept(detail::total_order_less<T, U>{}(lhs, rhs))) {
            return detail::total_order_less<T, U>{}(lhs, rhs);
        }
    };


}

#endif //COMPARE_H
