#define hsc_unsafe(name, args, res) GO(unsafe, name, args, res)
#define hsc_safe(name, args, res) GO(safe, name, args, res)

#define GO(safety, name, args, res) \
    extern CTYPE_##res name(CTYPE_##args);     \
    printf("%s\n", STR(foreign import ccall safety name :: HTYPE_##args HTYPE_##res));

#define STR(x) STR2(x)
#define STR2(x) #x

#define CTYPE_io(x) CTYPE_##x
#define CTYPE_ptr(x) CTYPE_##x*
#define CTYPE_unit void
#define CTYPE_0
#define CTYPE_1(x) CTYPE_##x
#define CTYPE_2(x,y) CTYPE_##x, CTYPE_##y
#define CTYPE_3(x,y,z) CTYPE_##x, CTYPE_##y, CTYPE_##z
#define CTYPE_4(x,y,z,w) CTYPE_##x, CTYPE_##y, CTYPE_##z, CTYPE_##w

#define HTYPE_io(x) IO (HTYPE_##x)
#define HTYPE_ptr(x) Ptr (HTYPE_##x)
#define HTYPE_unit ()
#define HTYPE_0
#define HTYPE_1(x) HTYPE_##x ->
#define HTYPE_2(x,y) HTYPE_##x -> HTYPE_##y ->
#define HTYPE_3(x,y,z) HTYPE_##x -> HTYPE_##y -> HTYPE_##z ->
#define HTYPE_4(x,y,z,w) HTYPE_##x -> HTYPE_##y -> HTYPE_##z -> HTYPE_##w ->
