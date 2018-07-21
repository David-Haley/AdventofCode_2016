with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with GNAT.MD5;

procedure December_14 is

   Input_File : File_Type;
   My_Input : Unbounded_String;
   subtype Hashes is String (1 .. 32);
   subtype Hex_Digits is Character
     with Static_Predicate => Hex_Digits in '0' .. '9' | 'a' .. 'f';
   Hash : Hashes;
   I : Natural := 0;
   Maximum_Hashes : constant Natural := 1000;
   type Hash_Buffer_Indices is mod Maximum_Hashes;
   Hash_Buffer : array (Hash_Buffer_Indices) of Hashes;
   Search_Digit : Hex_Digits;
   subtype Key_Counts is Natural range 0 .. 64;
   Key_Count : Key_Counts := Key_Counts'First;
   subtype Part_2_Cycles is Natural range 1 .. 2016;

   function Run_Length_3 (Hash : in Hashes; Hex_Digit : out Hex_Digits)
                          return Boolean is
      Found : Boolean;

   begin -- Run_Length_3
      for I in Hash'First .. Hash'Last - 2 loop
         Hex_Digit :=  Hash (I);
         Found := Hex_Digit = Hash (I + 1) and Hex_Digit = Hash (I + 2);
         exit when Found;
      end loop;
      return Found;
   end Run_Length_3;

   function Run_Length_5 (Hash : in Hashes; Hex_Digit : in Hex_Digits)
                          return Boolean is
      Found : Boolean;

   begin -- Run_Length_5
      for I in Hash'First .. Hash'Last - 4 loop
         Found := Hex_Digit =  Hash (I) and Hex_Digit = Hash (I + 1) and
           Hex_Digit = Hash (I + 2) and Hex_Digit = Hash (I + 3) and
           Hex_Digit = Hash (I + 4);
         exit when Found;
      end loop;
      return Found;
   end Run_Length_5;

begin -- December_14
   Open (Input_File, In_File, "20161214.txt");
   Get_Line (Input_File, My_Input);
   Close (Input_File);
   -- populate buffer with hashes 0 to 999
   for I in Hash_Buffer_Indices loop
      Hash_Buffer (I) :=
        GNAT.MD5.Digest (To_String (My_Input) &
                           Trim (Hash_Buffer_Indices'Image (I) , Left));
   end loop; -- I in Hash_Buffer_Indices
   loop -- Part one key search
      Hash := Hash_Buffer (Hash_Buffer_Indices (I mod Maximum_Hashes));
      Hash_Buffer (Hash_Buffer_Indices ((I + Maximum_Hashes)
                   mod Maximum_Hashes)) :=
        GNAT.MD5.Digest (To_String (My_Input) &
                           Trim (Natural'Image (I + Maximum_Hashes) , Left));
      if Run_Length_3 (Hash, Search_Digit) then
         For J in Hash_Buffer_Indices loop
            if Run_Length_5 (Hash_Buffer (J), Search_Digit) then
               Key_Count := Key_Count + 1;
               exit;
            end if; -- Run_Length_5 (Hash_Buffer (J), Search_Digit)
         end loop; -- J in Hash_Buffer_Indices
      end if; --  Run_Length_3 (Hash, Search_Digit)
      exit when Key_Count = Key_Counts'Last;
      I := I + 1;
   end loop; -- Part one key search
   Put_Line ("Index of last key:" & Natural'Image (I));
   -- Part Two re hash 2016 times!
   I := 0;
   Key_Count := Key_Counts'First;
   -- populate buffer with hashes 0 to 999
   for I in Hash_Buffer_Indices loop
      Hash_Buffer (I) :=
        GNAT.MD5.Digest (To_String (My_Input) &
                           Trim (Hash_Buffer_Indices'Image (I) , Left));
      for Hash_Cycle in Part_2_Cycles loop
         Hash_Buffer (I) := GNAT.MD5.Digest (Hash_Buffer (I));
      end loop; -- Hash_Cycle in Part_2_Cycles
   end loop; -- I in Hash_Buffer_Indices
   loop -- Part two key search
      Hash := Hash_Buffer (Hash_Buffer_Indices (I mod Maximum_Hashes));
      Hash_Buffer (Hash_Buffer_Indices ((I + Maximum_Hashes)
                   mod Maximum_Hashes)) :=
        GNAT.MD5.Digest (To_String (My_Input) &
                           Trim (Natural'Image (I + Maximum_Hashes) , Left));
      for Hash_Cycle in Part_2_Cycles loop
         Hash_Buffer (Hash_Buffer_Indices ((I + Maximum_Hashes)
                      mod Maximum_Hashes)) :=
           GNAT.MD5.Digest (Hash_Buffer (Hash_Buffer_Indices (
                            (I + Maximum_Hashes) mod Maximum_Hashes)));
      end loop; -- Hash_Cycle in Part_2_Cycles
      if Run_Length_3 (Hash, Search_Digit) then
         For J in Hash_Buffer_Indices loop
            if Run_Length_5 (Hash_Buffer (J), Search_Digit) then
               Key_Count := Key_Count + 1;
               exit;
            end if; -- Run_Length_5 (Hash_Buffer (J), Search_Digit)
         end loop; -- J in Hash_Buffer_Indices
      end if; -- Run_Length_3 (Hash, Search_Digit)
      exit when Key_Count = Key_Counts'Last;
      I := I + 1;
   end loop; -- Part two key search
   Put_Line ("Part two index of last key:" & Natural'Image (I));
 end December_14;
