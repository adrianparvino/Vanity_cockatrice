#include <caml/mlvalues.h>

CAMLprim value vanity_cockatrice_base32(value dest, value src);
CAMLprim value vanity_cockatrice_broadcast(value scratch, intnat start, intnat chunk, intnat n);
