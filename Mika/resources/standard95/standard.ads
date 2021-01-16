--  Representation of package Standard

--  This is not accurate Ada, since new base types cannot be 
--  created, but the listing shows the target dependent
--  characteristics of the Standard types for this compiler

package Standard is
pragma Pure(Standard);

   type Boolean is (False, True);
   for Boolean'Size use 1;
   for Boolean use (False => 0, True => 1);

   type Integer is range -(2 **31) .. +(2 **31 - 1);
   for Integer'Size use 32;

   subtype Natural  is Integer range 0 .. Integer'Last;
   subtype Positive is Integer range 1 .. Integer'Last;

   type Short_Short_Integer is range -(2 **7) .. +(2 **7 - 1);
   for Short_Short_Integer'Size use 8;

   type Short_Integer is range -(2 **15) .. +(2 **15 - 1);
   for Short_Integer'Size use 16;

   type Long_Integer is range -(2 **31) .. +(2 **31 - 1);
   for Long_Integer'Size use 32;

   type Long_Long_Integer is range -(2 **63) .. +(2 **63 - 1);
   for Long_Long_Integer'Size use 64;

   type Short_Float is digits 6
     range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;
   for Short_Float'Size use 32;

   type Float is digits 6
     range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;
   for Float'Size use 32;

   type Long_Float is digits 15
     range -16#0.FFFF_FFFF_FFFF_F8#E+256 .. 16#0.FFFF_FFFF_FFFF_F8#E+256;
   for Long_Float'Size use 64;

   type Long_Long_Float is digits 18
     range -16#0.FFFF_FFFF_FFFF_FFFF#E+4096 .. 16#0.FFFF_FFFF_FFFF_FFFF#E+4096;
   for Long_Long_Float'Size use 96;

type Character is
         (nul,  soh,  stx,  etx,     eot,  enq,  ack,  bel,
          bs,   ht,   lf,   vt,      ff,   cr,   so,   si,

          dle,  dc1,  dc2,  dc3,     dc4,  nak,  syn,  etb,
          can,  em,   sub,  esc,     fs,   gs,   rs,   us,

          ' ',  '!',  '"', '#',     '$',  '%',  '&',  ''',
          '(',  ')',  '*',  '+',     ',',  '-',  '.',  '/',

          '0',  '1',  '2',  '3',     '4',  '5',  '6',  '7',
          '8',  '9',  ':',  ';',     '<',  '=',  '>',  '?',

          '@',  'A',  'B',  'C',     'D',  'E',  'F',  'G',
          'H',  'I',  'J',  'K',     'L',  'M',  'N',  'O',

          'P',  'Q',  'R',  'S',     'T',  'U',  'V',  'W',
          'X',  'Y',  'Z',  '[',     '\',  ']',  '^',  '_',

          '`',  'a',  'b',  'c',     'd',  'e',  'f',  'g',
          'h',  'i',  'j',  'k',     'l',  'm',  'n',  'o',

          'p',  'q',  'r',  's',     't',  'u',  'v',  'w',
          'x',  'y',  'z',  '{',     '|',  '}',  '~',  del,

          reserved_128,     reserved_129,  bph,  nbh, reserved_132,     nel,     ssa,  esa,

          hts,  htj,  vts,  pld,     plu,  ri,   ss2,  ss3,

          dcs,  pu1,  pu2,  sts,     cch,  mw,   spa,  epa,

          sos, reserved_153, sci, csi, st,   osc,  pm,   apc,
          ' ',     '¡',   '¢',    '£',      '¤',   '¥',    '¦',   '§',   --160 (16#A0#) .. 167 (16#A7#)
      '¨',     '©',   'ª',    '«',      '¬',   '­',    '®',   '¯',   --168 (16#A8#) .. 175 (16#AF#)

      '°',     '±',   '²',    '³',      '´',   'µ',    '¶',   '·',   --176 (16#B0#) .. 183 (16#B7#)
      '¸',     '¹',   'º',    '»',      '¼',   '½',    '¾',   '¿',   --184 (16#B8#) .. 191 (16#BF#)

      'À',     'Á',   'Â',    'Ã',      'Ä',   'Å',    'Æ',   'Ç',   --192 (16#C0#) .. 199 (16#C7#)
      'È',     'É',   'Ê',    'Ë',      'Ì',   'Í',    'Î',   'Ï',   --200 (16#C8#) .. 207 (16#CF#)

      'Ð',     'Ñ',   'Ò',    'Ó',      'Ô',   'Õ',    'Ö',   '×',   --208 (16#D0#) .. 215 (16#D7#)
      'Ø',     'Ù',   'Ú',    'Û',      'Ü',   'Ý',    'Þ',   'ß',   --216 (16#D8#) .. 223 (16#DF#)

      'à',     'á',   'â',    'ã',      'ä',   'å',    'æ',   'ç',   --224 (16#E0#) .. 231 (16#E7#)
      'è',     'é',   'ê',    'ë',      'ì',   'í',    'î',   'ï',   --232 (16#E8#) .. 239 (16#EF#)

      'ð',     'ñ',   'ò',    'ó',      'ô',   'õ',    'ö',   '÷',   --240 (16#F0#) .. 247 (16#F7#)
      'ø',     'ù',   'ú',    'û',      'ü',   'ý',    'þ',   'ÿ'
          
 --    
          ); 
   for Character'Size use 8;
   --Wide_Character has 65536 positions, the first 256 positions have the same contents as type Character.
   --Mika approximates Wide_characters as derived type from Characters
   type Wide_Character is new Character;    --  See RM A.1(36) for details of this type
   for Wide_Character'Size use 16;

   type Wide_Wide_Character is new Wide_Character;
   for Wide_Wide_Character'Size use 32;

--
package ASCII is

      --  Control characters:
-- 
      NUL_ascii   : constant Character := Character'Val (16#00#);
      SOH_ascii   : constant Character := Character'Val (16#01#);
      STX_ascii   : constant Character := Character'Val (16#02#);
      ETX_ascii   : constant Character := Character'Val (16#03#);
      EOT_ascii   : constant Character := Character'Val (16#04#);
      ENQ_ascii   : constant Character := Character'Val (16#05#);
      ACK_ascii   : constant Character := Character'Val (16#06#);
      BEL_ascii   : constant Character := Character'Val (16#07#);
      BS_ascii    : constant Character := Character'Val (16#08#);
      HT_ascii    : constant Character := Character'Val (16#09#);
      LF_ascii    : constant Character := Character'Val (16#0A#);
      VT_ascii    : constant Character := Character'Val (16#0B#);
      FF_ascii    : constant Character := Character'Val (16#0C#);
      CR_ascii    : constant Character := Character'Val (16#0D#);
      SO_ascii    : constant Character := Character'Val (16#0E#);
      SI_ascii    : constant Character := Character'Val (16#0F#);
      DLE_ascii   : constant Character := Character'Val (16#10#);
      DC1_ascii   : constant Character := Character'Val (16#11#);
      DC2_ascii   : constant Character := Character'Val (16#12#);
      DC3_ascii   : constant Character := Character'Val (16#13#);
      DC4_ascii   : constant Character := Character'Val (16#14#);
      NAK_ascii   : constant Character := Character'Val (16#15#);
      SYN_ascii   : constant Character := Character'Val (16#16#);
      ETB_ascii   : constant Character := Character'Val (16#17#);
      CAN_ascii   : constant Character := Character'Val (16#18#);
      EM_ascii    : constant Character := Character'Val (16#19#);
      SUB_ascii   : constant Character := Character'Val (16#1A#);
      ESC_ascii   : constant Character := Character'Val (16#1B#);
      FS_ascii    : constant Character := Character'Val (16#1C#);
      GS_ascii    : constant Character := Character'Val (16#1D#);
      RS_ascii    : constant Character := Character'Val (16#1E#);
      US_ascii    : constant Character := Character'Val (16#1F#);
      DEL_ascii   : constant Character := Character'Val (16#7F#);

      -- Other characters:
      Space      : constant Character := ' ';
      Exclam     : constant Character := '!';
      Quotation  : constant Character := '"';
      Sharp      : constant Character := '#';
      Dollar     : constant Character := '$';
      Percent    : constant Character := '%';
      Ampersand  : constant Character := '&';
      Colon      : constant Character := ':';
      Semicolon  : constant Character := ';';
      Query      : constant Character := '?';
      At_Sign    : constant Character := '@';
      L_Bracket  : constant Character := '[';
      Back_Slash : constant Character := '\';
      R_Bracket  : constant Character := ']';
      Circumflex : constant Character := '^';
      Underline  : constant Character := '_';
      Grave      : constant Character := '`';
      L_Brace    : constant Character := '{';
      Bar        : constant Character := '|';
      R_Brace    : constant Character := '}';
      Tilde      : constant Character := '~';

      -- Lower case letters:

      LC_A : constant Character := 'a';
      LC_B : constant Character := 'b';
      LC_C : constant Character := 'c';
      LC_D : constant Character := 'd';
      LC_E : constant Character := 'e';
      LC_F : constant Character := 'f';
      LC_G : constant Character := 'g';
      LC_H : constant Character := 'h';
      LC_I : constant Character := 'i';
      LC_J : constant Character := 'j';
      LC_K : constant Character := 'k';
      LC_L : constant Character := 'l';
      LC_M : constant Character := 'm';
      LC_N : constant Character := 'n';
      LC_O : constant Character := 'o';
      LC_P : constant Character := 'p';
      LC_Q : constant Character := 'q';
      LC_R : constant Character := 'r';
      LC_S : constant Character := 's';
      LC_T : constant Character := 't';
      LC_U : constant Character := 'u';
      LC_V : constant Character := 'v';
      LC_W : constant Character := 'w';
      LC_X : constant Character := 'x';
      LC_Y : constant Character := 'y';
      LC_Z : constant Character := 'z';

   end ASCII;

   type String is array (Positive range <>) of Character;
   pragma Pack (String);

   type Wide_String is array (Positive range <>) of Wide_Character;
   pragma Pack (Wide_String);

   type Wide_Wide_String is array (Positive range <>) of Wide_Wide_Character;
   pragma Pack (Wide_Wide_String);

   type Duration is delta 0.000000001
     range -((2.0 ** 63 - 1) * 0.000000001) ..  
           +((2.0 ** 63 - 1) * 0.000000001);    --2 changed to 2.0 to avoid overlow
   for Duration'Small use 0.000000001;

   Constraint_Error : exception;
   Program_Error    : exception;
   Storage_Error    : exception;
   Tasking_Error    : exception;
   Numeric_Error    : exception renames Constraint_Error;

end Standard;
