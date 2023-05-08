#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <endian.h>
#include <stdint.h>
#include <x86intrin.h>
#include <string.h>

CAMLprim value vanity_cockatrice_base32(value dest, value src) {
  char *dest_c = Bytes_val(dest);
  char *src_c = Bytes_val(src);

  __m128i x = _mm_loadu_si64((__m128i *) src_c);
  __m128i mask = _mm_setr_epi8(
    0, 0,
    1, 0,
    1, 1,
    2, 1,
    3, 2,
    3, 3,
    4, 3,
    4, 4
  );
  x = _mm_shuffle_epi8(x, mask);
  __m128i shifts = _mm_setr_epi16(
    1 << (40 - 40),
    1 << (40 - 35),
    1 << (32 - 30),
    1 << (32 - 25),
    1 << (24 - 20),
    1 << (16 - 15),
    1 << (16 - 10),
    1 << (8 - 5)
  );

  x = _mm_mullo_epi16(x, shifts);
  x = _mm_srli_epi16(x, 11);
  x = _mm_packus_epi16(x, x);

  mask = _mm_cmpgt_epi8 (x, _mm_set1_epi8(9));
  x = _mm_add_epi8(x, _mm_set1_epi8('0'));
  x = _mm_add_epi8(x, _mm_and_si128(mask, _mm_set1_epi8('a' - '0' - 10)));

  _mm_storeu_si64((__m128i_u *) dest_c, x);

  return Val_unit;
}

CAMLprim value vanity_cockatrice_broadcast(value scratch, value s, intnat n, intnat pos) {
  char *scratch_c = Bytes_val(scratch);
  char *s_c = Bytes_val(s);
  int len = caml_string_length(s);

  char *start = scratch_c + pos;

  scratch_c[pos++] = ';';
  scratch_c[pos++] = 'S';
  scratch_c[pos++] = 'B';
  scratch_c[pos++] = ':';

  memcpy(scratch_c + pos, s_c, len);
  pos += len;

  char *write = scratch_c + pos;
  __asm__ __volatile__ ( "rep movsb" : "+D" (write) : "S" (start), "c" ((n - 1)*(len + 4)) : "memory" );

  return Val_int(write - scratch_c);
}