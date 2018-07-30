with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Assertions; use Ada.Assertions;

procedure December_21 is

   type Hash_Indices is mod 8;

   subtype Hash_Characters is Character
     with Static_Predicate => Hash_Characters in 'a' .. 'h';

   type Hashes is array (Hash_Indices) of Hash_Characters;

   Initial_Hash : constant Hashes := ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h');

   Hash : Hashes;

   Input_File : File_Type;

   function To_String (Hash : in Hashes) return String is

      Result : String (1 .. Positive (Hashes'Last - Hashes'First) + 1);

   begin -- To_String
      for I in Hash_Indices loop
         Result (Natural (I) + 1) := Hash (I);
      end loop; -- I in Hash_Indices
      return Result;
   end To_String;

   procedure Process (Hash : in out Hashes; Input_File : in out File_Type) is

      type Rotations is (Left, Right);

      Hash_Set : constant Character_Set := To_Set (To_String (Initial_Hash));

      Swap_Position_Text : constant String := "swap position";
      Swap_Letter_Text : constant String := "swap letter";
      Rotate_Left_Text : constant String := "rotate left";
      Rotate_Right_Text : constant String := "rotate right";
      Rotate_Letter_Text : constant String :=
        "rotate based on position of letter";
      Reverse_Positions_Text : constant String := "reverse positions";
      Move_Positions_Text : constant String := "move position";
      With_Letter_Text : constant String := "with letter";
      With_Position_Text : constant String := "with position";
      Through_Text : constant String := "through";
      To_Position_Text : constant String := "to position";

      Text : Unbounded_String;

      Start_At : Positive;

      Numeric_Operand_1, Numeric_Operand_2 : Hash_Indices;
      Letter_Operand_1, Letter_Operand_2 : Hash_Characters;

      function Letter_Operand (Text : in Unbounded_String;
                               Start_At : in out Positive)
                               return Hash_Characters is

         First : Positive;
         Last : Natural;

      begin -- Letter_Operand
         Find_Token (Text, Hash_Set, Start_At, Inside, First, Last);
         Assert (First = Last, "Bad character operand at line:" &
                   Positive_Count'Image (Line (Input_File)));
         Start_At := Last + 1;
         return Element (Text, First);
      end Letter_Operand;

      function Position_Operand (Text : in Unbounded_String;
                                 Start_At : in out Positive)
                                 return Hash_Indices is

         First : Positive;
         Last : Natural;

      begin -- Position_Operand
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Assert (Last /= 0, "Bad position operand at line:" &
                   Positive_Count'Image (Line (Input_File)));
         Start_At := Last + 1;
         return Hash_Indices'Value (Slice (Text, First, Last));
      end Position_Operand;

      procedure Skip (Text : in Unbounded_String;
                      To_Skip : in String;
                      Start_At : in out Positive) is
         Last : Natural;

      begin -- Skip
         Last := Index (Text, To_Skip, Start_At, Forward);
         Assert (Last /= 0, To_Skip & " not found at line:" &
                   Positive_Count'Image (Line (Input_File)));
         Start_At := Last + To_Skip'Length + 1;
      end Skip;

      procedure Swap (Hash : in out Hashes;
                      Numeric_Operand_1, Numeric_Operand_2 : in Hash_Indices) is

         Temp : Hash_Characters;

      begin -- Swap
         Temp := Hash (Numeric_Operand_2);
         Hash (Numeric_Operand_2) := Hash (Numeric_Operand_1);
         Hash (Numeric_Operand_1) := Temp;
      end Swap;

      procedure Rotate (Hash : in out Hashes; Way : in Rotations;
                        Steps : in Hash_Indices) is

         Rotated : Hashes;

      begin -- Rotate
         if Way = Left then
            for I in Hash_Indices loop
               Rotated (I) := Hash (I + Steps);
            end loop; -- I in Hash_Indices
         else
            for I in Hash_Indices loop
               Rotated (I + Steps) := Hash (I);
            end loop; --  I in Hash_Indices
         end if; -- Way = Left
         Hash := Rotated;
      end Rotate;

      procedure Mirror (Hash : in out Hashes;
                        Numeric_Operand_1,
                        Numeric_Operand_2 : in Hash_Indices) is

         Mirrored : Hashes := Hash;

      begin -- Mirror
         for I in Hash_Indices range Numeric_Operand_1 .. Numeric_Operand_2 loop
            Mirrored (Numeric_Operand_2 - I + Numeric_Operand_1) := Hash (I);
         end loop; -- I in Numeric_Operand_1 .. Numeric_Operand_2
         Hash := Mirrored;
      end Mirror;

      procedure Move (Hash : in out Hashes;
                      Numeric_Operand_1, Numeric_Operand_2 : in Hash_Indices) is

         Temp : Hash_Characters;
         I : Hash_Indices := Numeric_Operand_1;

      begin -- Move
         Temp := Hash (Numeric_Operand_1);
         if Numeric_Operand_2 > Numeric_Operand_1 then
            while I /= Numeric_Operand_2 loop
               Hash (I) := Hash (I + 1);
               I := I + 1;
            end loop; -- I /= Numeric_Operand_2
         else
            while I /= Numeric_Operand_2 loop
               Hash (I) := Hash (I - 1);
               I := I - 1;
            end loop; -- I /= Numeric_Operand_2
         end if; --
         Hash (Numeric_Operand_2) := Temp;
      end Move;

      function Index (Hash : in Hashes; Find : in Hash_Characters)
                      return Hash_Indices is

      begin -- Index
         for I in Hash_Indices loop
            if Hash (I) = Find then
               return I;
            end if; -- Hash (I) = Find
         end loop; --I in Hash_Indices
         Assert (False, Find & " not found in " & To_String (Hash));
         return Hash_Indices'First; -- unreachable
      end Index;

      procedure Swap (Hash : in out Hashes;
                      Letter_Operand_1,
                      Letter_Operand_2 : in Hash_Characters) is

      begin -- Swap
         Swap (Hash, Index (Hash, Letter_Operand_1),
               Index (Hash,Letter_Operand_2));
      end Swap;

      procedure Rotate (Hash : in out Hashes; Based : in Hash_Characters) is

         Steps : Hash_Indices := Index (Hash, Based);

      begin -- Rotate
         if Steps >= 4 then
            Steps := Steps + 1;
         end if; -- Steps >= 4
         Steps := Steps + 1;
         Rotate (Hash, Right, Steps);
      end Rotate;

   begin -- Process
      Reset (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Trim (Text, Left); -- remove leading blanks
         if 1 = Index (Text, Swap_Position_Text, Forward) then
            Start_At := Swap_Position_Text'Length + 1;
            Numeric_Operand_1 := Position_Operand (Text, Start_At);
            Skip (Text, With_Position_Text, Start_At);
            Numeric_Operand_2 := Position_Operand (Text, Start_At);
            Swap (Hash, Numeric_Operand_1, Numeric_Operand_2);
         elsif 1 = Index (Text, Swap_Letter_Text, Forward) then
            Start_At := Swap_Letter_Text'Length + 1;
            Letter_Operand_1 := Letter_Operand (Text, Start_At);
            Skip (Text, With_Letter_Text, Start_At);
            Letter_Operand_2 := Letter_Operand (Text, Start_At);
            Swap (Hash, Letter_Operand_1, Letter_Operand_2);
         elsif 1 = Index (Text, Rotate_Left_Text, Forward) then
            Start_At := Rotate_Left_Text'Length + 1;
            Numeric_Operand_1 := Position_Operand (Text, Start_At);
            Rotate (Hash, Left, Numeric_Operand_1);
         elsif 1 = Index (Text, Rotate_Right_Text, Forward) then
            Start_At := Rotate_Right_Text'Length + 1;
            Numeric_Operand_1 := Position_Operand (Text, Start_At);
            Rotate (Hash, Right, Numeric_Operand_1);
         elsif 1 = Index (Text, Rotate_Letter_Text, Forward) then
            Start_At := Rotate_Letter_Text'Length + 1;
            Letter_Operand_1 := Letter_Operand (Text, Start_At);
            Rotate (Hash, Letter_Operand_1);
         elsif 1 = Index (Text, Reverse_Positions_Text, Forward) then
            Start_At := Reverse_Positions_Text'Length + 1;
            Numeric_Operand_1 := Position_Operand (Text, Start_At);
            Skip (Text, Through_Text, Start_At);
            Numeric_Operand_2 := Position_Operand (Text, Start_At);
            Mirror (Hash, Numeric_Operand_1, Numeric_Operand_2);
         elsif 1 = Index (Text, Move_Positions_Text, Forward) then
            Start_At := Move_Positions_Text'Length + 1;
            Numeric_Operand_1 := Position_Operand (Text, Start_At);
            Skip (Text, To_Position_Text, Start_At);
            Numeric_Operand_2 := Position_Operand (Text, Start_At);
            Move (Hash, Numeric_Operand_1, Numeric_Operand_2);
         else
            Assert (False, "Bad operation at line:" &
                      Positive_Count'Image (Line (Input_File)));
         end if; -- determine operation
      end loop; -- not End_Of_File (Input_File)
   end Process;

   function Find_Password (Input_File : in out File_Type) return Hashes is

      Part_Two_Result : constant Hashes :=
        ('f', 'b', 'g', 'd', 'c', 'e', 'a', 'h');

      Password, Hash : Hashes;
      Repeated : Boolean;

   begin -- Find_Password
      for I0 in Hash_Characters loop
         Password (0) := I0;
         for I1 in Hash_Characters loop
            Password (1) := I1;
            for I2 in Hash_Characters loop
               Password (2) := I2;
               for I3 in Hash_Characters loop
                  Password (3) := I3;
                  for I4 in Hash_Characters loop
                     Password (4) := I4;
                     for I5 in Hash_Characters loop
                        Password (5) := I5;
                        for I6 in Hash_Characters loop
                           Password (6) := I6;
                           for I7 in Hash_Characters loop
                              Password (7) := I7;
                              Repeated := False;
                              for I in Hash_Indices range Hash_Indices'First ..
                                Hash_Indices'Last - 1 loop
                                 for J in Hash_Indices range I + 1 ..
                                   Hash_Indices'Last loop
                                    Repeated := Repeated or
                                      Password (I) = Password (J);
                                 end loop; -- J in Hash_Indices
                              end loop; --  I in Hash_Indices
                              if not Repeated then
                                 Hash := Password;
                                 Process (Hash, Input_File);
                                 if Hash = Part_Two_Result then
                                    return Password;
                                 end if; -- Hash = Part_Two_Result
                              end if; -- not Repeated
                           end loop; -- I7 in Hash_Characters
                        end loop; -- I6 in Hash_Characters
                     end loop; -- I5 in Hash_Characters
                  end loop; -- I4 in Hash_Characters
               end loop; -- I3 in Hash_Characters
            end loop; -- I2 in Hash_Characters
         end loop; -- I1 in Hash_Characters
      end loop; -- I0 in Hash_Characters
      return (others => 'f'); -- not valid
   end Find_Password;

begin -- December_21
   Open (Input_File, In_File, "20161221.txt");
   Hash := Initial_Hash;
   Process (Hash, Input_File);
   Put_Line ("Part one Hash: " & To_String (Hash));
   Put_Line ("Part two Password: " & To_String (Find_Password (Input_File)));
   Close (Input_File);
end December_21;
