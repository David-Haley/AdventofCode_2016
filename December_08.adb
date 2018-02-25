with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_08 is

   type X_Coordinates is mod 50;
   type Y_Coordinates is mod 6;

   type Screens is array (X_Coordinates, Y_Coordinates) of Boolean;

   procedure Put (Screen : in Screens) is

   begin -- Put
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            if Screen (X, Y) then
               Put ('#');
            else
               Put (' ');
            end if; -- Screen (X, Y)
         end loop; -- X in X_Coordinates
         New_Line;
      end loop; --Y in Y_Coordinates
   end Put;

   function Count_Pixels return Natural is

      procedure Rectangle (Screen : in out Screens;
                           X_Size : in X_Coordinates;
                           Y_Size : in Y_Coordinates) is

      begin -- Rectangle
         for X in X_Coordinates range 0 .. X_Size - 1 loop
            for Y in Y_Coordinates range 0 .. Y_Size - 1 loop
               Screen (X, Y) := True;
            end loop; -- Y in Y_Coordinates range 0 .. Y_Size - 1
         end loop; -- X in X_Coordinates range 0 .. X_Size - 1
      end Rectangle;

      procedure Rotate_Row (Screen : in out Screens;
                            Row : in Y_Coordinates;
                            Amount : in X_Coordinates) is

         Row_Buffer : array (X_Coordinates) of Boolean;

      begin -- Rotate_Row
         for X in X_Coordinates loop
            Row_Buffer (X + X_Coordinates (Amount)) := Screen (X, Row);
         end loop; -- X in X_Coordinates loop
         for X in X_Coordinates loop
            Screen (X, Row) := Row_Buffer (X);
         end loop; -- X in X_Coordinates loop
      end Rotate_Row;

      procedure Rotate_Column (Screen : in out Screens;
                               Column : in X_Coordinates;
                               Amount : in Y_Coordinates) is

         Column_Buffer : array (Y_Coordinates) of Boolean;

      begin -- Rotate_Column
         for Y in Y_Coordinates loop
            Column_Buffer (Y + Amount) := Screen (Column, Y);
         end loop; -- Y in Y_Coordinates
         for Y in Y_Coordinates loop
            Screen (Column, Y) := Column_Buffer (Y);
         end loop; -- Y in Y_Coordinates
      end Rotate_Column;

      Rect : constant String := "rect";
      Rotate : constant String := "rotate";
      Op_Code_Set : constant Character_Set := To_set (Rect & Rotate);

      Row : constant String := "row";
      Column : constant String := "column";
      Slice_Set : constant Character_Set := To_set (Row & Column);

      Input_File : File_Type;
      Input_Line : Unbounded_String;
      Result : Natural := 0;
      Screen : Screens := (others => (others => False));
      Start_At, First : Positive;
      Last : Natural;

      X, Row_Amount : X_Coordinates;
      Y, Column_Amount : Y_Coordinates;

   begin -- Count_Pixels
      Open (Input_File, In_File, "20161208.txt");
      while not End_Of_File (Input_File) loop
         Input_Line := To_Unbounded_String (Get_Line (Input_File));
         Start_At := 1;
         Find_Token (Input_Line, Op_Code_Set, Start_At, Inside, First, Last);
         Start_At := Last + 1;
         if Slice (Input_Line, First, Last) = Rect then
            Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            X := X_Coordinates'Value (Slice (Input_Line, First, Last));
            Start_At := Last + 1;
            Assert (Element (Input_Line, Start_At) = 'x',
                    "Expected 'x' and found: " &
                      Element (Input_Line, Start_At));
            Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Y := Y_Coordinates'Value (Slice (Input_Line, First, Last));
            Rectangle (Screen, X, Y);
         elsif Slice (Input_Line, First, Last) = Rotate then
            Find_Token (Input_Line, Slice_Set, Start_At, Inside, First, Last);
            Start_At := Last + 1;
            if Slice (Input_Line, First, Last) = Row then
               Find_Token (Input_Line, Decimal_Digit_Set, Start_at, Inside,
                           First, Last);
               Y := Y_Coordinates'Value (Slice (Input_Line, First, Last));
               Start_At := Last + 1;
               Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               Row_Amount := X_Coordinates'Value (Slice (Input_Line, First,
                                                  Last));
               Rotate_Row (Screen, Y, Row_Amount);
            elsif Slice (Input_Line, First, Last) = Column then
               Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               X := X_Coordinates'Value (Slice (Input_Line, First, Last));
               Start_At := Last + 1;
               Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               Column_Amount := Y_Coordinates'Value (Slice (Input_Line, First,
                                                     Last));
               Rotate_Column (Screen, X, Column_Amount);
            else
               Assert (False, "expected row or column, found: " &
                         Slice (Input_Line, First, Last));
            end if; -- Slice (Input_Line, First, Last) = Row
         else
            Assert (False, "Unknown OP_Code: " &
                      Slice (Input_Line, First, Last));
         end if; -- Slice (Input_Line, First, Last) = Rect
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      Put (Screen);
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            if Screen (X, Y) then
               Result := Result + 1;
            end if; -- Screen (X, Y)
         end loop; -- X in X_Coordinates
      end loop; --Y in Y_Coordinates
      return Result;
   end Count_Pixels;

begin  -- December_08
   Put_Line ("Count of lit pixels: " & Natural'Image (Count_Pixels));
end December_08;
