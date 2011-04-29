#define hsc_unsafe(...) hsc_go(unsafe, __VA_ARGS__)
#define hsc_safe(...) hsc_go(safe, __VA_ARGS__)
#define hsc_go(safety, purity, ret, name, ...) \
  extern TYPE_##ret(FST) name(TYPES(FST, COMMA, __VA_ARGS__)); \
  printf("%s\n", STR(DECLARATION(safety, purity, ret, name, __VA_ARGS__)));
#define STR(x) STR2(x)
#define STR2(x) #x
#define DECLARATION(safety, purity, ret, name, ...) \
  foreign import ccall safety name :: TYPES(SND, ARROW, __VA_ARGS__) SEP(ARROW, __VA_ARGS__) PURITY_##purity(TYPE_##ret(SND))
#define PURITY_io(x) IO x
#define PURITY_pure(x) x

#define TYPES(...) TYPES1(__VA_ARGS__)
#define TYPES1(type, sep, x, ...) TYPE_##x(type) SEP(sep, __VA_ARGS__) TYPES2(type, sep, __VA_ARGS__)
#define TYPES2(type, sep, x, ...) TYPE_##x(type) SEP(sep, __VA_ARGS__) TYPES3(type, sep, __VA_ARGS__)
#define TYPES3(type, sep, x, ...) TYPE_##x(type) SEP(sep, __VA_ARGS__) TYPES4(type, sep, __VA_ARGS__)
#define TYPES4(type, sep, x, ...) TYPE_##x(type) SEP(sep, __VA_ARGS__) TYPES5(type, sep, __VA_ARGS__)
#define TYPES5(type, sep, x, ...) TYPE_##x(type) SEP(sep, __VA_ARGS__) TYPES6(type, sep, __VA_ARGS__)
#define TYPES6(type, sep, x, ...) TYPE_##x(type) SEP(sep, __VA_ARGS__) TYPES7(type, sep, __VA_ARGS__)
#define TYPES7(type, sep, x, ...) TYPE_##x(type) SEP(sep, __VA_ARGS__) TYPES8(type, sep, __VA_ARGS__)
#define TYPES8(type, sep, x, ...) TYPE_##x(type)

#define SEP(sep, x, ...) TYPE_##x(sep)
#define COMMA(...) ,
#define ARROW(...) ->
#define FST(x,y) x
#define SND(x,y) y
#define TYPE_(f)
