with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;

procedure December_23 is

   Input_File, Output_File : File_Type;
   Program_Name : constant String := "Solution_23";
   Source_File_Path : constant String := "..\Source\";
   Assignment : constant String := " := ";
   Switch_String : constant String := "Switch";

   type Registers is (a, b, c, d);
   Register_Set : constant Character_Set := To_Set ("abcd");

   package Register_IO is new Ada.Text_IO.Enumeration_IO (Registers);
   use Register_IO;

   subtype Line_Numbers is Positive;
   Last_Line : Line_Numbers;
   Indent : Natural := 0;

   package Line_Number_IO is new Ada.Text_IO.Integer_IO (Line_Numbers);

   subtype Offsets is Integer range
     - Line_Numbers'Last .. Line_Numbers'Last;

   package Offset_IO is new Ada.Text_IO.Integer_IO (Offsets);
   use Offset_IO;

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   Integer_Set : constant Character_Set := To_Set ("+-0123456789");

   type Op_Codes is (cpy, dec, inc, jnz, tgl);
   Op_Code_Set : constant Character_Set := To_Set ("cpydecincjnztgl");

   package OP_Code_IO is new Ada.Text_IO.Enumeration_IO (Op_Codes);
   use Op_Code_IO;

   procedure Inc_Indent is

   begin -- Inc_Indent
      Indent := Indent + 1;
   end Inc_Indent;

   procedure Dec_Indent is

   begin -- Dec_Indent
      Indent := Indent - 1;
   end Dec_Indent;

   procedure Put_Indent (Output_File : in out File_Type) is

   begin -- Put_Indent
      Put (Output_File, Indent * "   ");
   end Put_Indent;

   procedure Put_Context (Output_File : in out File_Type) is

   begin -- Put_context
      Put_Line (Output_File, "with Ada.Text_IO; use Ada.Text_IO;");
      New_Line (Output_File);
      Put_Line (Output_File, "procedure " & Program_Name & " is");
      New_Line (Output_File);
   end Put_Context;

   procedure Put_Register (Output_File : in out File_type;
                           Register : in Registers) is

   begin -- Put_Register
      Put (Output_File, "Register_");
      Put (Output_File, Register);
   end Put_Register;

   procedure Put_Declarations (Input_File : in out File_Type;
                               Output_File : in out File_Type;
                               Last_Line : out Line_Numbers) is

      Instruction_Indices : constant String := "Instruction_Indices";

   begin -- Put_Declarations
      Inc_Indent;
         -- Count number of lines in the input file, every line needs a label
         -- because it is potentially a target of a jump instruction where the
         -- the target offset is specified by a register.
      while not End_Of_File (input_File) loop
         Last_Line := Line_Numbers (Line (Input_File));
         -- after the last line of actiual input is skipped the line count is
         -- reset to 1, presumably next page ?
         Skip_Line (Input_File);
      end loop; -- not End_Of_File (input_File)
      Reset (Input_File);
      for I in Registers loop
         Put_Indent (Output_File);
         Put_Register (Output_File, I);
         Put (Output_File, " : Integer" & Assignment);
         if I = a then
            Put_Line (Output_File, "7;");
         else
            Put_Line (Output_File, "0;");
         end if; -- I = a
      end loop; -- I in Registers
      New_Line (Output_File);
      Put_Indent (Output_File);
      Put_Line (Output_File,
                 "subtype " & Instruction_Indices & " is Positive range 1 .." &
                   Line_Numbers'Image (Last_Line) & ';');
      Put_Indent (Output_File);
      Put_Line (Output_File, Switch_String & " : array (" &
                  Instruction_Indices &
                  ") of Boolean := (others => True);");
      New_Line (Output_File);
      Dec_Indent;
   end Put_Declarations;

   procedure Put_Body (Input_File, Output_File : in out File_Type) is

      Label_Prefix : constant String := "Label_";
      Null_Statement : constant String := "null;";

      procedure Put_Label (Output_File : in out File_Type;
                           Line_Number : in Line_Numbers) is

      begin -- Put_Label
         Put_Indent (Output_File);
         Put (Output_File, "<<" & Label_Prefix);
         Line_Number_IO.Put (Output_File, Line_Number, 0);
         Put (Output_File, ">>");
      end Put_Label;

      procedure Get_Register_or_Immediate (Input_Line : in Unbounded_String;
                                           Is_Register : out Boolean;
                                           Register : out Registers;
                                           Number : out Integer;
                                           Start_At : in out Positive) is

         Register_First, Immediate_First : Positive;
         Register_Last, Immediate_Last : Natural;

      begin -- Get_Register_or_Immediate
         Find_Token (Input_Line, Integer_Set, Start_At, Inside, Immediate_First,
                     Immediate_Last);
         Find_Token (Input_Line, Register_Set, Start_At, Inside, Register_First,
                     Register_Last);
         Is_Register := Register_Last > 0 and then
           (Immediate_Last = 0 or Immediate_Last > Register_Last);
         if Is_Register then
            Get (Slice (Input_Line, Register_First, Register_Last), Register,
                 Register_Last);
            Start_At := Register_Last + 1;
            Number := 0; -- avoids undefined return value
         else
            Number := Integer'Value (Slice (Input_Line, Immediate_First,
                                     Immediate_Last));
            Start_At := Immediate_Last + 1;
            Register := Registers'First; -- avoids undefined return
         end if; -- Is_Register
      end Get_Register_or_Immediate;

      procedure Put_If_Switch (Output_File : in out File_Type;
                               Line_Number : in Line_Numbers) is

      begin -- Put_If_Switch
         Put_Indent (Output_File);
         Put (Output_File, "if " & Switch_String & " (");
         Int_IO.Put (Output_File, Line_Number, 0);
         Put_Line (Output_File, ") then");
         Inc_Indent;
      end Put_If_Switch;

      procedure Put_Else (Output_File : in out File_Type) is

      begin -- Put_Else
         Dec_Indent;
         Put_Indent (Output_File);
         Put_Line (Output_File, "else");
         Inc_Indent;
      end Put_Else;

      procedure Put_End_If (Output_File : in out File_Type) is

      begin -- Put_End_If
         Dec_Indent;
         Put_Indent (Output_File);
         Put_Line (Output_File, "end if;");
      end Put_End_If;

      procedure Put_cpy (Output_File : in out File_Type;
                         Input_Line : in Unbounded_String;
                         Line_Number : in Line_Numbers;
                         Start_At : in out Positive;
                         Last_Line : in Line_Numbers) is

         Is_Register, Always_Register : Boolean;
         Register, Source : Registers;
         Number, Immediate : Integer;

      begin -- Put_cpy
         Get_Register_or_Immediate (Input_Line, Is_Register, Source, Immediate,
                                    Start_At);
         Get_Register_or_Immediate (Input_Line, Always_Register, Register,
                                    Number, Start_At);
         Put_Indent (Output_File);
         if Always_Register then
            Put_Register (Output_File, Register);
            Put (Output_File, Assignment);
            if Is_Register then
               Put_Register (Output_File, Source);
            else
               Int_IO.Put (Output_File, Immediate, 0);
            end if; -- Is_Register
            Put_Line (Output_File, ";");
         else
            Put_Line (Output_File, Null_Statement);
         end if; -- Always_Register
      end Put_cpy;

      procedure Put_jnz (Output_File : in out File_Type;
                         Input_Line : in Unbounded_String;
                         Line_Number : in Line_Numbers;
                         Start_At : in out Positive;
                         Last_Line : in Line_Numbers) is

         Is_Register : Boolean;
         Register : Registers;
         Offset : Offsets;
         Immediate : Integer;

      begin -- Put_jnz
         Put_Indent (Output_File);
         Put (Output_File, "if ");
         Get_Register_or_Immediate (Input_Line, Is_Register, Register,
                                    Immediate, Start_At);
         if Is_Register then
            Put_Register (Output_File, Register);
         else
            Int_IO.Put (Output_File, Immediate, 0);
         end if; -- Is_Register
         Put_Line (Output_File, " /= 0 then");
         Inc_Indent;
         Get_Register_or_Immediate (Input_Line, Is_Register, Register, Offset,
                                    Start_At);
         if Is_Register then
            Put_Indent (Output_File);
            Put (Output_File, "case ");
            Put_Register (Output_File, Register);
            Put_Line (Output_File, " +" & Line_Numbers'Image(Line_Number) &
                        " is");
            Inc_Indent;
            for I in Line_Numbers range Line_Numbers'First .. Last_Line loop
               Put_Indent (Output_File);
               Put (Output_File, "when" & Line_Numbers'Image (I) & " => goto " &
                      Label_Prefix);
               Int_IO.Put (Output_File, I, 0);
               Put_Line (Output_File, ";");
            end loop; -- I in Line_Numbers range Line_Numbers'First .. Last_Line
            Put_Indent (Output_File);
            Put_Line (Output_File, "when others => goto Label_End;");
            Dec_Indent;
            Put_Indent (Output_File);
            Put (Output_File, "end case; -- ");
            Put_Register (Output_File, Register);
            New_Line (Output_File);
         else
            Put_Indent (Output_File);
            Put (Output_File, "goto " & Label_Prefix);
            Put (Output_File, Line_Numbers (Offsets (Line_Number) + Offset), 0);
            Put_Line (Output_File, ";");
         end if;
         Put_End_If (Output_File);
      end Put_jnz;

      procedure Put_tgl (Output_File : in out File_Type;
                         Input_Line : in Unbounded_String;
                         Line_Number : in Line_Numbers;
                         Start_At : in out Positive) is

         Is_Register : Boolean;
         Register : Registers;
         Offset : Offsets;

      begin -- Put_tgl
         Put_Indent (Output_File);
         Get_Register_or_Immediate (Input_Line, Is_Register, Register,
                                    Offset, Start_At);
         if Is_Register then
            Put (Output_File, "if 1 <= (");
            Put_Register (Output_File, Register);
            Put (Output_File," +" & Line_Numbers'Image (Line_Number) &
                   ") and (");
            Put_Register (Output_File, Register);
            Put (Output_File," +" & Line_Numbers'Image (Line_Number));
            Put_Line (Output_File, ") <=" & Line_Numbers'Image (Last_Line) &
                        " then");
            Inc_Indent;
            Put_Indent (Output_File);
            Put (Output_File, Switch_String & " (");
            Put (Output_File, Line_Number, 0);
            Put (Output_File, " + ");
            Put_Register (Output_File, Register);
            Put (Output_File, ") := not ");
            Put (Output_File, Switch_String & " (");
            Put (Output_File, Line_Number, 0);
            Put (Output_File, " + ");
            Put_Register (Output_File, Register);
            Put_Line (Output_File, ");");
            Put_Else (Output_File);
            Put_Indent (Output_File);
            Put_Line (Output_File, Null_Statement);
            Put_End_If (Output_File);
         else
            if 1 <= (Line_Number + Offset) and
              (Line_Number + Offset) <= Last_Line then
               Put (Output_File, Switch_String & " (");
               Put (Output_File, Line_Number + Offset, 0);
               Put (Output_File, ") := not ");
               Put (Output_File, Switch_String & " (");
               Put (Output_File, Line_Number + Offset, 0);
               Put_Line (Output_File, ");");
            else
               Put_Line (Output_File, Null_Statement);
            end if; -- Line_Number + Offset not in range 1 .. Last_Line
         end if; -- Is_Register
      end Put_tgl;

      procedure Put_Inc_or_Dec (Output_File : in out File_Type;
                                Input_Line : in Unbounded_String;
                                Line_Number : in Line_Numbers;
                                Op_Code : in Op_Codes;
                                Start_At : in out Positive;
                                Last_Line : in Line_Numbers) is

         Is_Register : Boolean;
         Register : Registers;
         Number : integer;

      begin -- Put_Inc_or_Dec
         Get_Register_or_Immediate (Input_Line, Is_Register, Register, Number,
                                    Start_At);
         Put_If_Switch (Output_File, Line_Number);
         Put_Indent (Output_File);
         Put_Register (Output_File, Register);
         Put (Output_File, Assignment);
         Put_Register (Output_File, Register);
         if Op_Code = Dec then
            Put_Line (Output_File, " - 1;");
         else
            Put_Line (Output_File, " + 1;");
         end if;
         Put_Else (Output_File);
         Put_Indent (Output_File);
         Put_Register (Output_File, Register);
         Put (Output_File, Assignment);
         Put_Register (Output_File, Register);
         if Op_Code = Dec then
            Put_Line (Output_File, " + 1;");
         else
            Put_Line (Output_File, " - 1;");
         end if;
         Put_End_If (Output_File);
      end Put_Inc_or_Dec;

      Input_Line : Unbounded_String;
      First, Start_At, Restart : Positive;
      Last : Natural;
      Line_Number : Line_Numbers;
      Op_Code : Op_Codes;

   begin -- Put_Body
      Put_Line (Output_File, "begin -- " & Program_Name);
      Inc_Indent;
      while not End_Of_File (input_File) loop
         Line_Number := Line_Numbers (Line (Input_File));
         Input_Line := Get_Line (Input_File);
         Put_Label (Output_File, Line_Number);
         Put (Output_File, " -- ");
         Put_Line (Output_File, Input_Line);
         Start_At := 1;
         Find_Token (Input_Line, Op_Code_Set, Start_At, Inside, First, Last);
         Get (Slice (Input_Line, First, Last), Op_Code, last);
         Start_At := Last + 1;
         Restart := Start_At;
         case Op_Code is
            when cpy =>
               Put_If_Switch (Output_File, Line_Number);
               Put_cpy (Output_File, Input_Line, Line_Number, Start_At,
                        Last_Line);
               Put_Else (Output_File);
               Put_jnz (Output_File, Input_Line, Line_Number, Restart,
                        Last_Line);
               Put_End_If (Output_File);
            when dec | inc  =>
               Put_Inc_or_Dec(Output_File, Input_Line, Line_Number, Op_Code,
                              Start_At, Last_Line);
            when jnz =>
               Put_If_Switch (Output_File, Line_Number);
               Put_jnz (Output_File, Input_Line, Line_Number, Start_At,
                        Last_Line);
               Put_Else (Output_File);
               Put_cpy (Output_File, Input_Line, Line_Number, Restart,
                        Last_Line);
               Put_End_If (Output_File);
            when tgl =>
               Put_If_Switch (Output_File, Line_Number);
               Put_tgl (Output_File, Input_Line, Line_Number, Start_At);
               Put_Else (Output_File);
               Put_Indent (Output_File);
               Put_Register (Output_File, a);
               Put (Output_File, Assignment);
               Put_Register (Output_File, a);
               Put_Line (Output_File, " + 1;");
               Put_End_If (Output_File);
         end case; -- Op_Code
      end loop; -- not End_Of_File (input_File)
      Put_Indent (Output_File);
      Put_Line (Output_File, "<<" & Label_Prefix & "End>>");
      Put_Indent (Output_File);
      Put (Output_File, "Put_Line (""");
      Put_Register (Output_File, a);
      Put (Output_File, ":"" & Integer'Image (");
      Put_Register (Output_File, a);
      Put_Line (Output_File, "));");
      Dec_Indent;
      Put_Line (Output_File, "end " & Program_Name & ';');
   end Put_Body;

begin -- December_23
   Open (Input_File, In_File, "20161223.txt");
   Create (Output_File, Out_file, Source_File_Path & Program_Name & ".adb");
   Put_Context (Output_File);
   -- first pass to count source lines
   Put_Declarations (Input_File, Output_File, Last_Line);
   -- second pass to generate code
   Put_Body (Input_File, Output_File);
   Close (Output_File);
   Close (Input_File);
end December_23;
