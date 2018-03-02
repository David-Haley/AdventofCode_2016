with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Assertions; use Ada.Assertions;

procedure December_10 is

   Input_File, Output_File : File_Type;
   Program_Name : constant String := "Solution_10";
   Source_File_Path : constant String := "..\Source\";

   subtype Values is Natural range 0 .. 99;

   package Input_Lists is new Ada.Containers.Doubly_Linked_Lists (Values);
   use Input_Lists;

   type Instructions is record
      Is_Output : Boolean := False;
      Destination : Unbounded_String := Null_Unbounded_String;
   end record; -- Instructions

   package Instruction_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Instructions);
   use Instruction_Lists;

   type Symbols is record
      Name : Unbounded_String;
      Input_List : Input_Lists.List := Input_Lists.Empty_list;
      Instruction_List : Instruction_Lists.List := Instruction_Lists.Empty_List;
   end record; -- Symbols

   function "<" (Left, Right : Symbols) return Boolean is

   begin -- "<"
      return Left.Name < Right.Name;
   end "<";

   function "=" (Left, Right : Symbols) return Boolean is

   begin -- "="
      return Left.Name = Right.Name;
   end "=";

   package Symbol_Tables is new Ada.Containers.Ordered_Sets (Symbols);
   use Symbol_Tables;

   function Key (Symbol : Symbols) return Unbounded_String is

   begin -- Key
     return Symbol.Name;
   end Key;

   package Symbol_Keys is new Generic_Keys (Unbounded_String, Key);
   use Symbol_Keys;

   Symbol_Table : Symbol_Tables.Set := Empty_Set;

   package Output_Lists is new Ada.Containers.Ordered_Sets (Unbounded_String);
   use Output_Lists;

   Output_List : Output_Lists.Set := Output_Lists.Empty_Set;

   Symbol_Prefix : constant String := "Bot_";
   Output_Prefix : constant String := "Output_";
   Indent_1 : constant String := "   ";
   Indent_2 : constant String := "      ";
   Indent_3 : constant String := "         ";
   Indent_4 : constant String := "            ";
   Indent_5 : constant String := "               ";
   Indent_6 : constant String := "                  ";
   Indent_7 : constant String := "                     ";

   procedure Put_Context (Output_File : in out File_Type) is

   begin -- Put_Context
      Put_Line (Output_File, "with Ada.Text_IO; use Ada.Text_IO;");
      New_Line (Output_File);
      Put_Line (Output_File, "procedure " & Program_Name & " is");
      New_Line (Output_File);
      Put_Line (Output_File, Indent_1 &
                  "subtype Values is Natural range 0 .. 99;");
      New_Line (Output_File);
      Put_Line (Output_File, Indent_1 & "type Inputs is record");
      Put_Line (Output_File, Indent_2 & "Value : Values := 0;");
      Put_Line (Output_File, Indent_2 & "Defined : Boolean := False;");
      Put_Line (Output_File, Indent_1 & "end record; -- Inputs");
      New_Line (Output_File);
      Put_Line (Output_File, Indent_1 &
                  "subtype Input_Indices is Natural range 0 .. 1;");
      New_Line (Output_File);
      Put_Line (Output_File, Indent_1 &
                  "type Input_Arrays is array (Input_Indices) of Inputs;");
      New_Line (Output_File);
   end Put_Context;

   procedure Build_Symbol_Table (Input_File : in out File_Type;
                                 Symbol_Table : in out Symbol_Tables.Set;
                                 Output_List : in out Output_Lists.Set) is

      Lexical_Bot : constant String := "bot";
      Lexical_Value : constant String := "value";
      Lexical_Gives : constant String := "goes to bot";
      Lexical_Output : constant String := "output";
      Lexical_Low : constant String := "gives low to";
      Lexical_High : constant String := "and high to";

      Input_Line, Bot_Low, Bot_High : Unbounded_String;
      First, Start_At : Positive;
      Last : Natural;
      Symbol : Symbols;
      Input : Values;
      Symbol_Cursor : Symbol_Tables.Cursor;
      Output_Cursor : Output_Lists.Cursor;
      Low_Instruction : Instructions := (False, Null_Unbounded_String);
      High_Instruction : Instructions := (False, Null_Unbounded_String);

   begin -- Build_Symbol_Table
      while not End_Of_File (input_File) loop
         Symbol.Input_List := Input_Lists.Empty_List;
         Symbol.Instruction_List := Instruction_Lists.Empty_List;
         Input_Line := To_Unbounded_String (Get_Line (Input_File));
         Start_At := 1;
         Find_Token (Input_Line, Letter_Set, Start_At, Inside, First, Last);
         Start_At := Last + 1;
         if Slice (Input_Line, First, Last) = Lexical_Bot then
            Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Symbol.Name := Symbol_Prefix &
              Unbounded_Slice (Input_Line, First, Last);
            Start_At := Last + 1;
            Last := Index (Input_Line, Lexical_Low, Start_At);
            Assert (Last /= 0, "Expected " & Lexical_Low & " : " &
                    To_String (Input_Line));
            Start_At := Last + Lexical_Low'Length;
            Find_Token (Input_Line, Letter_Set, Start_At, Inside, First,
                        Last);
            Start_At := Last + 1;
            if Slice (Input_Line, First, Last) = Lexical_Bot then
               Low_Instruction.Is_Output := False;
               Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               Low_Instruction.Destination := Symbol_Prefix &
                 Unbounded_Slice (Input_Line, First, Last);
               Start_At := Last + 1;
            elsif Slice (Input_Line, First, Last) = Lexical_Output then
               Low_Instruction.Is_Output := True;
               Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               Low_Instruction.Destination := Output_Prefix &
                 Unbounded_Slice (Input_Line, First, Last);
               Output_Cursor := Find (Output_List, Low_Instruction.Destination);
               If Output_Cursor = Output_Lists.No_Element then
                  Insert (Output_List, Low_Instruction.Destination);
               end if; -- Output_Cursor = Output_Lists.No_Element
               Start_At := Last + 1;
            else
               Assert (False, "Low " & Lexical_Bot & " | " & Lexical_Output &
                         " : " & To_String (Input_Line));
            end if; -- Slice (Input_Line, First, Last) = Lexical_Bot
            Last := Index (Input_Line, Lexical_High, Start_At);
            Assert (Last /= 0, "Expected " & Lexical_High & " : " &
                    To_String (Input_Line));
            Start_At := Last + Lexical_High'Length;
            Find_Token (Input_Line, Letter_Set, Start_At, Inside, First,
                        Last);
            Start_At := Last + 1;
            if Slice (Input_Line, First, Last) = Lexical_Bot then
               High_Instruction.Is_Output := False;
               Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               High_Instruction.Destination := Symbol_Prefix &
                 Unbounded_Slice (Input_Line, First, Last);
               Start_At := Last + 1;
            elsif Slice (Input_Line, First, Last) = Lexical_Output then
               High_Instruction.Is_Output := True;
               Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               High_Instruction.Destination := Output_Prefix &
                 Unbounded_Slice (Input_Line, First, Last);
               Output_Cursor := Find (Output_List, High_Instruction.Destination);
               If Output_Cursor = Output_Lists.No_Element then
                  Insert (Output_List, High_Instruction.Destination);
               end if; -- Output_Cursor = No_Element
            else
               Assert (False, "High " & Lexical_Bot & " | " & Lexical_Output &
                         " : " & To_String (Input_Line));
            end if; -- Slice (Input_Line, First, Last) = Lexical_Bot
            Symbol_Cursor := Find (Symbol_Table, Symbol.Name);
            if Symbol_Cursor /= Symbol_Tables.No_Element then
               Append (Symbol_Table (Symbol_Cursor).Instruction_List,
                       Low_Instruction);
               Append (Symbol_Table (Symbol_Cursor).Instruction_List,
                       High_Instruction);
            else
               Append (Symbol.Instruction_List, Low_Instruction);
               Append (Symbol.Instruction_List, High_Instruction);
               Insert (Symbol_Table, Symbol);
            end if; -- Symbol_Cursor /= Symbol_Tables.No_Element
         elsif Slice (Input_Line, First, Last) = Lexical_Value then
            Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Input := Values'Value (Slice (Input_Line, First, Last));
            Start_At := Last + 1;
            Last := Index (Input_Line, Lexical_Gives, Start_At);
            Assert (Last /= 0, "Expected " & Lexical_Gives & " destination:" &
                      To_String (Input_Line));
            Start_At := Last + Lexical_Gives'Length;
            Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Symbol.Name := Symbol_Prefix &
              Unbounded_Slice (Input_Line, First, Last);
            Symbol_Cursor := Find (Symbol_Table, Symbol.Name);
            if Symbol_Cursor /= Symbol_Tables.No_Element then
               Append (Symbol_Table (Symbol_Cursor).Input_List, Input);
            else
               Append (Symbol.Input_List, Input);
               Insert (Symbol_Table, Symbol);
            end if; -- Symbol_Cursor /= Symbol_Tables.No_Element
         else
            Assert (False, Lexical_Bot & " | " & Lexical_Value & " : " &
                      To_String (Input_Line));
         end if; -- Slice (Input_Line, First, Last) = Lexical_Bot
      end loop; -- not End_Of_File (input_File)
   end Build_Symbol_Table;

   procedure Put_Declarations (Output_File : in out File_Type;
                               Symbol_Table : in Symbol_Tables.Set;
                              Output_List : in Output_Lists.Set) is

   begin -- Put_Declarations
      for Cursor in Output_List.Iterate loop
         Put_Line (Output_File, To_String (Output_List(Cursor)) &
                     " : Values := 0;");
      end loop; -- Cursor in Output_List.Iterate;
      New_Line (Output_File);
      for Cursor in Symbol_Table.Iterate loop
         Put_Line (Output_File, Indent_1 & "task " &
                     To_String (Symbol_Table (Cursor).Name) & " is");
         Put_Line (Output_File, Indent_2 &
                     "entry Set_Input (Value : in Values);");
         Put_Line (Output_File, Indent_2 &
                     "entry End_Task;");
         Put_Line (Output_File, Indent_1 & "end; -- " &
                     To_String (Symbol_Table (Cursor).Name));
         New_Line (Output_File);
      end loop; -- Cursor in Symbol_Table.Iterate
      Put_Line (Output_File, Indent_1 & "task Master is");
      Put_Line (Output_File, Indent_2 &
                  "entry Found (Bot_Name : in String);");
      Put_Line (Output_File, Indent_1 & "end; -- Master");
      New_Line (Output_File);
   end Put_Declarations;

   procedure Put_Task (Output_File : in out File_Type;
                       Symbol : In Symbols) is

      Input_Cursor : Input_Lists.Cursor;
      Instruction_Cursor : Instruction_Lists.Cursor;

   begin -- Put_Task
      Put_Line (Output_File, Indent_1 & "task body " &
                  To_String (Symbol.Name) & " is");
      New_Line (Output_File);
      Put (Output_File, Indent_2 & "Input_Array : Input_Arrays");
      Input_Cursor := First (Symbol.Input_List);
      if Input_Cursor = Input_Lists.No_Element then
         Put_Line (Output_File, " := ((0, False), (0, False));");
      else
         Put (Output_File, " := ((" &
                Values'Image (Symbol.Input_List (Input_Cursor)) & ", True), ");
         Input_Cursor := Next (Input_Cursor);
         if Input_Cursor = Input_Lists.No_Element then
            Put_Line (Output_File, "(0, False));");
         else
            Put_Line (Output_File, '(' &
                        Values'Image (Symbol.Input_List (Input_Cursor)) & ", True));");
         end if; -- Input_Cursor = Input_Lists.No_Element
      end if; -- Input_Cursor = Input_Lists.No_Element
      Put_Line (Output_File, Indent_2 & "Run : Boolean := True;");
      New_Line (Output_File);
      Put_Line (Output_File, Indent_1 & "begin -- " & To_String (Symbol.Name));
      Put_Line (Output_File, Indent_2 &
                  "if Input_Array (0).Defined and Input_Array (1).Defined then");
      Put_Line (Output_File, Indent_3 &
                  "if Input_Array (0).Value < Input_Array (1).Value then");
      Instruction_Cursor := First (Symbol.Instruction_List);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File, Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     " := Input_Array (0).Value;"));
      else
         Put_Line (Output_File, Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (0).Value);"));
      end if; -- low not sent to output
      Instruction_Cursor := Next (Instruction_Cursor);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File,Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     " := Input_Array (1).Value;"));
      else
         Put_Line (Output_File,Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (1).Value);"));
      end if; -- Symbol.Instruction_List (Instruction_Cursor).Is_Output
      Put_Line (Output_File, Indent_3 & "else");
      Instruction_Cursor := First (Symbol.Instruction_List);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File, Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     " := Input_Array (1).Value;"));
      else
         Put_Line (Output_File, Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (1).Value);"));
      end if; -- Symbol.Instruction_List (Instruction_Cursor).Is_Output
      Instruction_Cursor := Next (Instruction_Cursor);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File, Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination) &
                     " := Input_Array (0).Value;");
      else
         Put_Line (Output_File,Indent_4 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (0).Value);"));
      end if; -- Symbol.Instruction_List (Instruction_Cursor).Is_Output
      Put_Line (Output_File, Indent_3 &
                  "end if; -- Input_Array (0).Value < Input_Array (1).Value");
      Put_Line (Output_File, Indent_3 & "Input_Array (0).Defined := False;");
      Put_Line (Output_File, Indent_3 & "Input_Array (1).Defined := False;");
      Put_Line (Output_File, Indent_2 &
                  "end if; -- Input_Array (0).Defined and Input_Array (1).Defined");
      Put_Line (Output_File, Indent_2 & "while Run loop");
      Put_Line (Output_File, Indent_3 & "select");
      Put_Line (Output_File, Indent_4 &
                  "accept Set_Input (Value : in Values) do");
      Put_Line (Output_File, Indent_5 & "if Input_Array (0).Defined then");
      Put_Line (Output_File, Indent_6 & "Input_Array (1).Value := Value;");
      Put_Line (Output_File, Indent_6 & "Input_Array (1).Defined := True;");
      Put_Line (Output_File, Indent_5 & "else");
      Put_Line (Output_File, Indent_6 & "Input_Array (0).Value := Value;");
      Put_Line (Output_File, Indent_6 & "Input_Array (0).Defined := True;");
      Put_Line (Output_File, Indent_5 & "end if; -- Input_Array (0).Defined;");
      Put_Line (Output_File, Indent_5 &
                  "if Input_Array (0).Defined and Input_Array (1).Defined then");
      Put_Line (Output_File, Indent_6 &
                  "if Input_Array (0).Value < Input_Array (1).Value then");
      Instruction_Cursor := First (Symbol.Instruction_List);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File, Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     " := Input_Array (0).Value;"));
      else
         Put_Line (Output_File, Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (0).Value);"));
      end if; -- Symbol.Instruction_List (Instruction_Cursor).Is_Output
      Instruction_Cursor := Next (Instruction_Cursor);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File,Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     " := Input_Array (1).Value;"));
      else
         Put_Line (Output_File,Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (1).Value);"));
      end if; --  Symbol.Instruction_List (Instruction_Cursor).Is_Output
      Put_Line (Output_File, Indent_6 & "else");
      Instruction_Cursor := First (Symbol.Instruction_List);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File, Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     " := Input_Array (1).Value;"));
      else
         Put_Line (Output_File, Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (1).Value);"));
      end if; -- Symbol.Instruction_List (Instruction_Cursor).Is_Output
      Instruction_Cursor := Next (Instruction_Cursor);
      if Symbol.Instruction_List (Instruction_Cursor).Is_Output then
         Put_Line (Output_File,Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     " := Input_Array (0).Value;"));
      else
         Put_Line (Output_File,Indent_7 & To_String (Symbol.Instruction_List
                   (Instruction_Cursor).Destination &
                     ".Set_Input (Input_Array (0).Value);"));
      end if; -- Symbol.Instruction_List (Instruction_Cursor).Is_Output
      Put_Line (Output_File, Indent_6 &
                  "end if; -- Input_Array (0).Value < Input_Array (1).Value");
      Put_Line (Output_File, Indent_6 &
                  "if (Input_Array (0).Value = 17 and Input_Array (1).Value = 61) or");
      Put_Line (Output_File, Indent_6 &
                  "  (Input_Array (1).Value = 17 and Input_Array (0).Value = 61) then");
      Put_Line (Output_File, Indent_7 &
                  "Master.Found (""" & To_String (Symbol.Name) & """);");
      Put_Line (Output_File, Indent_6 & "end if; -- solution found");
      Put_Line (Output_File, Indent_6 & "Input_Array (0).Defined := False;");
      Put_Line (Output_File, Indent_6 & "Input_Array (1).Defined := False;");
      Put_Line (Output_File, Indent_5 &
                  "end if; -- Input_Array (0).Defined and Input_Array (1).Defined");
      Put_Line (Output_File, Indent_4 & "end; -- Set_Input");
      Put_Line (Output_File, Indent_3 & "or");
      Put_Line (Output_File, Indent_4 & "accept End_Task do");
      Put_Line (Output_File, Indent_5 & "Run := False;");
      Put_Line (Output_File, Indent_4 & "end; -- End_Task");
      Put_Line (Output_File, Indent_3 & "end select;");
      Put_Line (Output_File, Indent_2 & "end loop; -- Run");
      Put_Line (Output_File, Indent_1 & "end " & To_String (Symbol.Name) & ';');
      New_Line (Output_File);
   end Put_Task;

   procedure Put_Master_Task (Output_File : in out File_Type;
                              Symbol_Table : in Symbol_Tables.Set) is

   begin -- Put_Master_Task
      Put_Line (Output_File, Indent_1 & "task body Master is");
      New_Line (Output_File);
      Put_Line (Output_File, Indent_1 & "begin -- Master");
      Put_Line (Output_File, Indent_2 &
                  "accept Found (Bot_Name : in String) do");
      Put_Line (Output_File, Indent_3 & "Put_Line (""Part one: "" & Bot_Name);");
      Put_Line (Output_File, Indent_2 & "end; -- Found");
      Put_Line (Output_File, Indent_2 &
                  "Put_Line (""Waiting for transfers to complete"");");
      Put_Line (Output_File, Indent_2 & "delay 3.0;");
      Put_Line (Output_File, Indent_2 &
                  "Put_Line (""Part two:"" & Natural'Image (" &
                  "Output_0 * Output_1 * Output_2));");
      for Cursor in Symbol_Table.Iterate loop
         Put_Line (Output_File, Indent_2 &
                     To_String (Symbol_Table (Cursor).Name) & ".End_Task;");
      end loop; --Cursor in Symbol_Table.Iterate
      Put_Line (Output_File, Indent_1 & "end; -- Master");
      New_Line (Output_File);
   end Put_Master_Task;

   procedure Put_Body (Output_File : in out File_Type;
                       Symbol_Table : in Symbol_Tables.Set) is

   begin -- Put_Body
      for Cursor in Symbol_Table.Iterate loop
         Put_Task (Output_File, Symbol_Table (Cursor));
      end loop; -- Cursor in Symbol_Table.Iterate
      Put_Master_Task (Output_File, Symbol_Table);
      Put_Line (Output_File, "begin -- " & Program_Name);
      Put_Line (Output_File, Indent_1 & "Put_Line (""Running Bots"");");
      Put_Line(Output_File, "end " & Program_Name & ';');
   end Put_Body;

begin -- December_10
   Open (Input_File, In_File, "20161210.txt");
   Build_Symbol_Table (Input_File, Symbol_Table, Output_List);
   Close (Input_File);
   Create (Output_File, Out_file, Source_File_Path & Program_Name & ".adb");
   Put_Context (Output_File);
   Put_Declarations (Output_File, Symbol_Table, Output_List);
   Put_Body ( Output_File, Symbol_Table);
   Close (Output_File);
end December_10;
