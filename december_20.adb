with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Interfaces; use Interfaces;
with Ada.Containers.Vectors;

procedure December_20 is

   subtype IP_Addersses is Unsigned_32;

   type Black_List_Items is record
      Lower, Upper : IP_Addersses;
   end record; -- Discs

   subtype Item_Numbers is Positive;

   package Black_Lists is new Ada.Containers.Vectors (Item_Numbers,
                                                      Black_List_Items);
   use Black_Lists;

   function "<" (Left, Right : in Black_List_Items) return Boolean;

   package Black_List_Sort is new Generic_Sorting ;
   use Black_List_Sort;

   Black_List : Black_Lists.Vector := Empty_Vector;

   function "<" (Left, Right : in Black_List_Items) return Boolean is

   begin -- "<"
      return Left.Lower < Right.Lower;
   end "<";

   procedure Read_Input (Black_List : in out Black_Lists.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Black_List_Item : Black_List_Items;

   begin -- Read_Input
      Open (Input_File, In_File, "20161220.txt");
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Black_List_Item.Lower := IP_Addersses'Value (Slice (Text, First,
                                                      Last));
         Start_At := Last + 1;
         Start_At := Index (Text, "-", Start_At) + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Black_List_Item.Upper := IP_Addersses'Value (Slice (Text, First,
                                                      Last));
         Append (Black_List, Black_List_Item);
      end loop; --not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function First_White (Black_List : in Black_Lists.Vector)
                         return IP_Addersses is

      Highest_Black : IP_Addersses := IP_Addersses'First;

   begin -- First_White
      Assert (Is_Sorted (Black_List), "List not sorted");
      for I in Item_Numbers range
        First_Index (Black_List) .. Last_Index (Black_List) loop
         if Black_List (I).Lower > 0 and then
           Black_List (I).Lower - 1 > Highest_Black then
            -- the second part of the test will fail when
            -- Black_List (I).Lower = 0
            return Highest_Black + 1;
         end if; -- at least one address available
         if Black_List (I).Upper > Highest_Black then
            Highest_Black := Black_List (I).Upper;
         end if; -- Black_List (I).Upper > Highest_Black
      end loop; -- I in Item_Numbers range 1 .. Last_Index (Black_List)
      assert (False, "No white addresses exist");
      return 0; -- unreachable code
   end First_White;

   function White_Count (Black_List : in Black_Lists.Vector) return Natural is

      Highest_Black : IP_Addersses := IP_Addersses'First;
      Result : Natural := 0;

   begin -- White_Count
      Assert (Is_Sorted (Black_List), "List not sorted");
      for I in Item_Numbers range
        First_Index (Black_List) .. Last_Index (Black_List) loop
         if Black_List (I).Lower > Highest_Black then
            -- This test is a little weak, see test in First_White it returns a
            -- correct answer because if two Black_List items are directly
            -- adjoining 0 is addes to the white count
            Result := Result +
              Natural (Black_List (I).Lower - Highest_Black) - 1;
         end if; --  Black_List (I).Lower - 1 > Highest_Black
         if Black_List (I).Upper > Highest_Black then
            Highest_Black := Black_List (I).Upper;
         end if; -- Black_List (I).Upper > Highest_Black
      end loop; -- I in Item_Numbers range First_Index .. Last_Index
      if IP_Addersses'Last > Highest_Black then
         Result := Result + Natural (IP_Addersses'Last - Highest_Black);
      end if; -- Addersses'Last > Highest_Black
      return Result;
   end White_Count;

begin -- December_20
   Read_Input (Black_List);
   Sort (Black_List);
   Put_Line ("First usable address:" &
               IP_Addersses'Image (First_White (Black_List)));
   Put_Line ("Useable addresses:" & Natural'Image (White_Count(Black_List)));
end December_20;
