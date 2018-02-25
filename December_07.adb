with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_07 is

   function Count_TLS_Addresses return Natural is

      Input_File : File_Type;
      Input_Line : Unbounded_String;
      Result : Natural := 0;
      Bracket_Count : Natural;
      ABBA_Inside_Brackets, ABBA_Outside_Brackets : Boolean;

   begin -- Count_TLS_Addresses
      Open (Input_File, In_File, "20161207.txt");
      while not End_Of_File (Input_File) loop
         Input_Line := To_Unbounded_String (Get_Line (Input_File));
         Bracket_Count := 0;
         ABBA_Inside_Brackets := False;
         ABBA_Outside_Brackets := False;
         for Index in Positive range 1 .. Length (Input_Line) - 3 loop
            case Element (Input_Line, Index) is
               when '[' =>
                  Bracket_Count := Bracket_Count + 1;
               when ']' =>
                  Bracket_Count := Bracket_Count - 1;
               when others =>
                  if Bracket_Count = 0 then
                     ABBA_Outside_Brackets := ABBA_Outside_Brackets or
                       (Element (Input_Line, Index) =
                            Element (Input_Line, Index + 3) and
                            Element (Input_Line, Index + 1) =
                            Element (Input_Line, Index + 2) and
                            Element (Input_Line, Index) /=
                            Element (Input_Line, Index + 1));
                  else
                     ABBA_Inside_Brackets := ABBA_Inside_Brackets or
                       (Element (Input_Line, Index) =
                            Element (Input_Line, Index + 3) and
                            Element (Input_Line, Index + 1) =
                            Element (Input_Line, Index + 2) and
                            Element (Input_Line, Index) /=
                            Element (Input_Line, Index + 1));
                  end if; -- not inside brackets and is ABBA
            end case; -- Element (Input_Line, Index)
         end loop; -- Index in Positive range 1 .. Length (Input_Line) - 3
         if ABBA_Outside_Brackets and not ABBA_Inside_Brackets then
            Result := Result + 1;
         end if; -- ABBA_Outside_Brackets and not ABBA_Inside_Brackets
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      return Result;
   end Count_TLS_Addresses;

   function Count_SSL_Addresses return Natural is

      Input_File : File_Type;
      Input_Line : Unbounded_String;
      Result : Natural := 0;
      Bracket_Count, BAB_Bracket_Count, Start_At, First : Natural;
      Search_BAB : String (1 .. 3);
      BAB_Found : Boolean;

   begin -- Count_SSL_Addresses
      Open (Input_File, In_File, "20161207.txt");
      while not End_Of_File (Input_File) loop
         Input_Line := To_Unbounded_String (Get_Line (Input_File));
         Bracket_Count := 0;
         for Pos in Positive range 1 .. Length (Input_Line) loop
            BAB_Found := False;
            case Element (Input_Line, Pos) is
               when '[' =>
                  Bracket_Count := Bracket_Count + 1;
               when ']' =>
                  Bracket_Count := Bracket_Count - 1;
               when others =>
                  if Bracket_Count = 0 and Pos > 2 then
                     if Element (Input_Line, Pos) =
                       Element (Input_Line, Pos - 2) and
                       Element (Input_Line, Pos) /=
                       Element (Input_Line, Pos - 1) then
                        -- ABA found
                        Search_BAB (1) := Element (Input_Line, Pos - 1);
                        Search_BAB (2) := Element (Input_Line, Pos);
                        Search_BAB (3) := Element (Input_Line, Pos - 1);
                        Start_At := 1;
                        while Start_At > 0 and not BAB_Found loop
                           First := Index (Input_Line, Search_BAB, Start_At);
                           if First > 0 then
                              BAB_Bracket_Count := 0;
                              for I in Positive range 1 .. First loop
                                 if Element (Input_Line, I) = '[' then
                                    BAB_Bracket_Count := BAB_Bracket_Count + 1;
                                 elsif Element (Input_Line, I) = ']' then
                                    BAB_Bracket_Count := BAB_Bracket_Count - 1;
                                 end if; -- Element (Input_Line, I) = '['
                              end loop; -- I in Positive range 1 .. First
                              if BAB_Bracket_Count > 0 then
                                 Result := Result + 1; -- BAB inside []
                                 BAB_Found := True;
                              end if; -- Bracket_Count > 0
                              Start_At := First + 1;
                           else
                              Start_At := 0;
                           end if; -- Count (Input_Line, Search_BAB) > 0 then
                        end loop; -- Start_At > 0
                     end if; -- is ABA
                  end if; -- not inside brackets and is ABBA
            end case; -- Element (Input_Line, Pos)
            exit when BAB_Found;
         end loop; -- Pos in Positive range 1 .. Length (Input_Line)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      return Result;
   end Count_SSL_Addresses;

begin  -- December_07
   Put_Line ("Count of TLS addersses: " & Natural'Image (Count_TLS_Addresses));
   Put_Line ("Count of SSL addersses: " & Natural'Image (Count_SSL_Addresses));
end December_07;
