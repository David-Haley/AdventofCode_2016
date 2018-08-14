with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Assertions; use Ada.Assertions;

procedure December_25 is

   Input_File, Output_File : File_Type;
   Program_Name : constant String := "Solution_25";
   Source_File_Path : constant String := "..\Source\";
   Assignment : constant String := " := ";

   type Registers is (a, b, c, d);
   Register_Set : constant Character_Set := To_Set ("abcd");

   package Register_IO is new Ada.Text_IO.Enumeration_IO (Registers);
   use Register_IO;

   subtype Line_Numbers is Positive;
   Indent : Natural := 0;

   package Line_Number_IO is new Ada.Text_IO.Integer_IO (Line_Numbers);

   subtype Offsets is Integer range
     - Line_Numbers'Last .. Line_Numbers'Last;

   package Offset_IO is new Ada.Text_IO.Integer_IO (Offsets);
   use Offset_IO;

   Integer_Set : constant Character_Set := To_Set ("+-0123456789");

   type Op_Codes is (cpy, dec, inc, jnz, out_op);
   Op_Code_Set : constant Character_Set := To_Set ("cpydecincjnzout_");

   package OP_Code_IO is new Ada.Text_IO.Enumeration_IO (Op_Codes);
   use Op_Code_IO;

   package Jump_Tables is new Ada.Containers.Ordered_Sets (Line_Numbers);
   use Jump_Tables;

   Jump_Table : Jump_Tables.Set := Empty_Set;
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

   procedure Build_Jump_Table (Input_File : in out File_Type;
                               Jump_Table : in out Jump_Tables.Set) is

      Input_Line : Unbounded_String;
      Op_Code : Op_Codes;
      Line_Number : Line_Numbers;
      Start_At, First : Positive;
      Offset : Offsets;
      Last : Natural;
      Register_First, Immediate_First : Positive;
      Register_Last, Immediate_Last : Natural;

   begin -- Build_Jump_Table
      while not End_Of_File (input_File) loop
         Line_Number := Line_Numbers (Line (Input_File));
         Input_Line := Get_Line (Input_File);
         Start_At := 1;
         Find_Token (Input_Line, Op_Code_Set, Start_At, Inside, First, Last);
         Get (Slice (Input_Line, First, Last), Op_Code, Start_At);
         if Op_Code = jnz then
            Find_Token (Input_Line, Integer_Set, Start_At, Inside,
                        Immediate_First, Immediate_Last);
            Find_Token (Input_Line, Register_Set, Start_At, Inside,
                        Register_First, Register_Last);
            if Register_Last > 0 then
               Start_AT := Register_Last + 1;
            else
              Start_At := Immediate_Last + 1;
            end if; --  Register_Last > 0
            Find_Token (Input_Line, Integer_Set, Start_At, Inside, First, Last);
            Get (Slice (Input_Line, First, Last), Offset, Start_At);
            Line_Number := Line_Numbers (Offsets (Line_Number) + Offset);
            if not Contains (Jump_Table, Line_Number) then
               Include (Jump_Table, Line_Number);
            end if; -- not Contains (Jump_Table, Line_Number)
         end if; -- Op_Code = jnz
      end loop; -- not End_Of_File (input_File)
   end Build_Jump_Table;

   procedure Put_Register (Output_File : in out File_type;
                           Register : in Registers) is

   begin -- Put_Register
      Put (Output_File, "Register_");
      Put (Output_File, Register);
   end Put_Register;

   procedure Put_Declarations (Output_File : in out File_Type) is

   begin -- Put_Declarations
      Inc_Indent;
      for I in Registers loop
         Put_Indent (Output_File);
         Put_Register (Output_File, I);
         Put_Line (Output_File, " : Integer;");
      end loop; -- I in Registers
      Put_Indent (Output_File);
      Put_Line (Output_File, "Test_A : Integer" & Assignment & "1;");
      Put_Indent (Output_File);
      Put_Line (Output_File, "Previous_Out : Integer;");
      Put_Indent (Output_File);
      Put_Line (Output_File, "Cycle : Natural" & Assignment & "0;");

      New_Line (Output_File);
      Dec_Indent;
   end Put_Declarations;

   procedure Put_Body (Input_File, Output_File : in out File_Type) is

      Label_Prefix : constant String := "Label_";

      procedure Put_Label (Output_File : in out File_Type;
                           Line_Number : in Line_Numbers) is

      begin -- Put_Label
         Put_Indent (Output_File);
         Put (Output_File, "<<" & Label_Prefix);
         Line_Number_IO.Put (Output_File, Line_Number, 0);
         Put_Line (Output_File, ">>");
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

      procedure Put_goto (Output_File : in out File_Type;
                          Input_Line : in Unbounded_String;
                          Line_Number : in Line_Numbers;
                          Start_At : in out Positive) is

         First : Positive;
         Last : Natural;
         Offset : Offsets;

      begin -- Put_goto
         Put (Output_File, "goto " & Label_Prefix);
         Find_Token (Input_Line, Integer_Set, Start_At, Inside, First, Last);
         Get (Slice (Input_Line, First, Last), Offset, Start_At);
         Put (Output_File, Line_Numbers (Offsets (Line_Number) + Offset), 0);
         Put_Line (Output_File, ";");
      end Put_goto;

      Input_Line : Unbounded_String;
      First, Start_At : Positive;
      Last : Natural;
      Line_Number : Line_Numbers;
      Op_Code : Op_Codes;
      Source, Register : Registers;
      Number, Immediate : Integer;
      Is_Register, Always_Register : Boolean;

   begin -- Put_Body
      Put_Line (Output_File, "begin -- " & Program_Name);
      Inc_Indent;
      Put_Indent (Output_File);
      Put_Line (Output_File, "while Cycle < 1000 loop");
      Inc_Indent;
      Put_Indent (Output_File);
      Put_Register (Output_File, a);
      Put_Line (Output_File, Assignment & "Test_A;");
      Put_Indent (Output_File);
      Put_Register (Output_File, b);
      Put_Line (Output_File, Assignment & "0;");
      Put_Indent (Output_File);
      Put_Register (Output_File, c);
      Put_Line (Output_File, Assignment & "0;");
      Put_Indent (Output_File);
      Put_Register (Output_File, d);
      Put_Line (Output_File, Assignment & "0;");
      Put_Indent (Output_File);
      Put_Line (Output_File, "Cycle" & Assignment & "0;");
      Put_Indent (Output_File);
      Put_Line (Output_File, "Previous_Out" & Assignment & "1;");
      while not End_Of_File (input_File) loop
         Line_Number := Line_Numbers (Line (Input_File));
         Input_Line := Get_Line (Input_File);
         Put_Indent (Output_File);
         Put (Output_File, "--");
         Put (Output_File, Line_Number, 4);
         Put_Line (Output_File, ": " & Input_Line);
         if Contains (Jump_Table, Line_Number) then
            Put_Label (Output_File, Line_Number);
         end if; -- Contains (Jump_Table, Line_Number)
         Start_At := 1;
         Find_Token (Input_Line, Op_Code_Set, Start_At, Inside, First, Last);
         Get (Slice (Input_Line, First, Last), Op_Code, last);
         Start_At := Last + 1;
         case Op_Code is
            when cpy =>
               Get_Register_or_Immediate (Input_Line, Is_Register, Source,
                                          Immediate, Start_At);
               Get_Register_or_Immediate (Input_Line, Always_Register,
                                          Register, Number, Start_At);
               Put_Indent (Output_File);
               Put_Register (Output_File, Register);
               Put (Output_File, Assignment);
               if Is_Register then
                  Put_Register (Output_File, Source);
               else
                  Put (Output_File, Integer'Image (Immediate));
               end if; -- Is_Register
               Put_Line (Output_File, ";");
            when dec | inc =>
               Get_Register_or_Immediate (Input_Line, Is_Register, Register,
                                          Number, Start_At);
               Put_Indent (Output_File);
               Put_Register (Output_File, Register);
               Put (Output_File, Assignment);
               Put_Register (Output_File, Register);
               if Op_Code = dec then
                  Put_Line (Output_File, " - 1;");
               elsif Op_Code = inc then
                  Put_Line (Output_File, " + 1;");
               end if; -- Op_Code = dec
            when jnz =>
               Put_Indent (Output_File);
               Put (Output_File, "if ");
               Get_Register_or_Immediate (Input_Line, Is_Register, Register,
                                          Immediate, Start_At);
               if Is_Register then
                  Put_Register (Output_File, Register);
               else
                  Put (Output_File, Integer'Image (Immediate));
               end if; -- Is_Register
               Put_Line (Output_File, " /= 0 then");
               Inc_Indent;
               Put_Indent (Output_File);
               Put_goto (Output_File, Input_Line, Line_Number, Start_At);
               Dec_Indent;
               Put_Indent (Output_File);
               Put_Line (Output_File, "end if;");
            when out_op =>
               Get_Register_or_Immediate (Input_Line, Always_Register,
                                          Register, Number, Start_At);
               Put_Indent (Output_File);
               Put (Output_File, "if ");
               Put_Register (Output_File, Register);
               Put_Line (Output_File, " = Previous_Out or Cycle > 1000 then");
               Inc_Indent;
               Put_Indent (Output_File);
               Put_Line (Output_File, "goto Label_End;");
               Dec_Indent;
               Put_Indent (Output_File);
               Put_Line (Output_File, "end if;");
               Put_Indent (Output_File);
               Put (Output_File, "Previous_Out" & Assignment);
               Put_Register (Output_File, Register);
               Put_Line (Output_File, ";");
               Put_Indent (Output_File);
               Put_Line (Output_File, "Cycle" & Assignment & "Cycle + 1;");
         end case; -- Op_Code
      end loop; -- not End_Of_File (input_File)
      if Last_Element (Jump_Table) > Line_Number then
         Put_Label (Output_File, Last_Element (Jump_Table));
      end if; -- Last_Element (Jump_Table) > Line_Number
      Put_Indent (Output_File);
      Put_Line (Output_File, "<<Label_End>>");
      Put_Indent (Output_File);
      Put_Line (Output_File, "if Cycle < 1000 then");
      Inc_Indent;
      Put_Indent (Output_File);
      Put_Line (Output_File, "Test_A" & Assignment & "Test_A + 1;");
      Dec_Indent;
      Put_Indent (Output_File);
      Put_Line (Output_File, "end if;");
      Dec_Indent;
      Put_Indent (Output_File);
      Put_Line (Output_File, "end loop;");
      Put_Indent (Output_File);
      Put_Line (Output_File,
                "Put_Line (""A value:"" & Positive'Image (Test_A));");
      Dec_Indent;
      Put_Line(Output_File, "end " & Program_Name & ';');
   end Put_Body;

begin -- December_25
   Open (Input_File, In_File, "20161225.txt");
   Create (Output_File, Out_file, Source_File_Path & Program_Name & ".adb");
   Put_Context (Output_File);
   -- first pass to build jump table
   Build_Jump_Table (Input_File, Jump_Table);
   Put_Declarations (Output_File);
   Reset (Input_File);
   -- second pass to generate code
   Put_Body (Input_File, Output_File);
   Close (Output_File);
   Close (Input_File);
end December_25;
