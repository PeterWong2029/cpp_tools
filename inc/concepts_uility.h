
#ifndef CONCEPTS_UILITY_H
#define CONCEPTS_UILITY_H
#include <concepts>
#include <memory>
#include <type_traits>

namespace urlicht {
	namespace concepts {

	template <typename T>
	concept decays_to_ptr =
			std::is_pointer_v<std::decay_t<T>>;

	template <typename T, typename Rng>
	concept CompatibleRange =
            std::ranges::input_range<Rng> &&
            std::constructible_from<T, std::ranges::range_reference_t<Rng>>;

    template <typename T, typename Iter>
	concept CompatibleIterator =
            std::forward_iterator<Iter> &&
            std::constructible_from<T, std::iter_reference_t<Iter>>;


    template <typename Alloc>
    concept Allocator = std::default_initializable<Alloc> && requires {
    typename Alloc::value_type;
    // Note the only type alias required from Alloc is value_type. std::allocator_traits generates
    // the remaining ones (e.g. pointer, size_type) accordingly.
    }
	&& requires (Alloc alloc,
            typename std::allocator_traits<Alloc>::pointer p,
            typename std::allocator_traits<Alloc>::size_type n) {
    { std::allocator_traits<Alloc>::allocate(alloc, n) }
    -> std::same_as<typename std::allocator_traits<Alloc>::pointer>;
    { std::allocator_traits<Alloc>::deallocate(alloc, p, n) }
    -> std::same_as<void>;
#if __cplusplus >= 202302L
    { std::allocator_traits<Alloc>::allocate_at_least(alloc, n) }
    -> std::same_as<std::allocation_result<
            typename std::allocator_traits<Alloc>::pointer,
            typename std::allocator_traits<Alloc>::size_type>>;
#endif
    // std::allocator_traits provides fallback methods for the remaining static methods (e.g. max_size )
    };

    // Minimum requirement of a container
    template <typename Cont>
    concept Container =
        std::same_as<Cont, std::remove_reference_t<Cont>> &&
        std::default_initializable<Cont> &&
        std::constructible_from<Cont, const Cont&> &&
        std::constructible_from<Cont, Cont&&> &&
    requires {
      	typename Cont::value_type;
        typename Cont::size_type;
        typename Cont::difference_type;
        typename Cont::reference;
        typename Cont::const_reference;
        typename Cont::pointer;
        typename Cont::const_pointer;
        typename Cont::iterator;
        typename Cont::const_iterator;
    }
    && requires(const Cont& cont, Cont& cont2) {
        // Assignment
        { cont = cont2 } -> std::same_as<Cont&>;
        { cont = std::move(cont2) } -> std::same_as<Cont&>;
        // Capacity
    	{ cont.empty() } -> std::convertible_to<bool>;
        { cont.max_size() } -> std::convertible_to<typename Cont::size_type>;
		// Iterators
        { cont.begin() } -> std::convertible_to<typename Cont::const_iterator>;
        { cont.cbegin() } -> std::same_as<typename Cont::const_iterator>;
        { cont.end() } -> std::convertible_to<typename Cont::const_iterator>;
        { cont.cend() } -> std::same_as<typename Cont::const_iterator>;
    };


}
}

#endif //CONCEPTS_UILITY_H
