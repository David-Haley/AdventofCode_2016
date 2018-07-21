with Ada.Text_IO; use Ada.Text_IO;

package body December_13_Common is

   Designer_Number : Unsigned_32;
   Input_File : File_Type;

   package Unsigned_IO is new Ada.Text_IO.Modular_IO (Unsigned_32);
   use Unsigned_IO;

   function Is_Open_Space (X, Y : in Coordinates) return Boolean is

      Bit_Mask : constant Coordinates := 1;
      Wall_Test : Unsigned_32;
      One_Count : Natural range 0 .. Coordinates'Size := 0;
      -- uses global velue Designer_Number

   begin -- Is_Open_Space
      Wall_Test := X ** 2 + 3 * X + 2 * X * Y + Y + Y ** 2 + Designer_Number;
      for I in Positive range 1 .. Unsigned_32'Size loop
         if (Wall_Test and Bit_Mask) = Bit_Mask then
            One_Count := One_Count + 1;
         end if; -- Wall_Test and Bit_Mask = Bit_Mask
         Wall_Test := Shift_Right (Wall_Test, 1);
      end loop; -- I in range 1 .. Coordinates'Size
      return One_Count mod 2 = 0; -- even numbers of 1s represent open space
   end Is_Open_Space;

begin -- December_13_Common
   Open (Input_File, In_file, "20161213.txt");
   Get (Input_File, Designer_Number);
   Close (Input_File);
end December_13_Common;
