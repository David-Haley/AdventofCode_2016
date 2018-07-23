with Ada.Assertions; use Ada.Assertions;

package body December_16_Library is

   function Invert (From : in Character) return Character is

   begin -- Invert
      if From = '0' then
         return '1';
      elsif From = '1' then
         return '0';
      else
         Assert (False, "not in binary digit set: " & From);
         return From; -- unreachable
      end if;
   end Invert;

end December_16_Library;
