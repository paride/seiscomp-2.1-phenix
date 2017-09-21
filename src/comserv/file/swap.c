
#define  REG     register                                                                                                                                         #define rchar    REG __s8
#define rshort   REG __s16
#define rint     REG __s32                                                                          #define rlong    REG __s32                                                                    #define ruchar   REG __u8                                                                           #define rushort  REG __u16
#define ruint    REG __u32                                                                       #define rulong   REG __u32
 
#define  swapbyte( ch )  ( ( ( (ch) << 4 ) | ( (ch) >> 4 ) ) )


#define HI_BYTE(x) ( *( ( __u8 *)(&x)+1 ) )
#define LO_BYTE(x) ( *( ( __u8 *)&x ) )
 
#define HI_WORD(x) ( *( ( __u16 *)(&x)+1 ) )
#define LO_WORD(x) ( *( ( __u16 *)&x ) )
 
#ifndef MAKEWORD
#define MAKEWORD(lo, hi)    ((__u16) (((__u16) lo) | ((__u16) hi << 8)))
#endif
 
#ifndef MAKELONG
#define MAKELONG(lo, hi)    ((__u32) (((__u32) lo) | ((__u32) hi << 16)))
#endif
#define SwapWords(dWord)        ((__u32) ((dWord >> 16) | (dWord << 16)))
#define SwapBytes(word)         ((__u16) ((word >> 8) | (word << 8)))    
