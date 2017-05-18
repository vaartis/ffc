#ifdef HAVE_OPTIONAL
    #include <optional>
    using std::optional;
    using std::nullopt;
#else
    #include <experimental/optional>
    using std::experimental::optional;
    using std::experimental::nullopt;
#endif
