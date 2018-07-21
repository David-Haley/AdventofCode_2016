with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;

procedure December_12 is

   Input_File, Output_File : File_Type;
   Program_Name : constant String := "Solution_12";
   Source_File_Path : constant String := "..\Source\";
   Indent_1 : constant String := "   ";
   Indent_2 : constant String := "      ";
   Assignment : constant String := " := ";

   type Registers is (a, b, c, d);
   Register_Set : constant Character_Set := To_Set ("abcd");

   package Register_IO is new Ada.Text_IO.Enumeration_IO (Registers);
   use Register_IO;

   subtype Line_Numbers is Natural;

   package Line_Number_IO is new Ada.Text_IO.Integer_IO (Line_Numbers);

   subtype Offsets is Integer range
     - Line_Numbers'Last .. Line_Numbers'Last;

   package Offset_IO is new Ada.Text_IO.Integer_IO (Offsets);
   use Offset_IO;

   Integer_Set : constant Character_Set := To_Set ("+-0123456789");

   type Op_Codes is (cpy, dec, inc, jnz);
   Op_Code_Set : constant Character_Set := To_Set ("cpydecincjnz");

   package OP_Code_IO is new Ada.Text_IO.Enumeration_IO (Op_Codes);
   use Op_Code_IO;

   package Jump_Tables is new Ada.Containers.Ordered_Sets (Line_Numbers);
   use Jump_Tables;

   Jump_Table : Jump_Tables.Set := Empty_Set;

   procedure Put_Context (Output_File : in out File_Type) is

   begin -- Put_context
      Put_Line (Output_File, "with Ada.Text_IO; use Ada.Text_IO;");
      New_Line (Output_File);
      Put_Line (Output_File, "procedure " & Program_Name & " is");
      New_Line (Output_File);
   end Put_Context;

   procedure Build_Jump_Table (Input_File : in out File_Type;
                                 Jump_Table : in out Jump_Tables.Set) is

      Line_Number : Line_Numbers;
      Op_Code : Op_Codes;
      Offset : Offsets;
      Input_Line : Unbounded_String;
      First, Start_At : Positive;
      Last : Natural;

   begin -- Build_Jump_Table
      while not End_Of_File (input_File) loop
         Line_Number := Line_Numbers (Line (Input_File));
         Input_Line := To_Unbounded_String (Get_Line (Input_File));
         Start_At := 1;
         Find_Token (Input_Line, Op_Code_Set, Start_At, Inside, First, Last);
         Get (Slice (Input_Line, First, Last), Op_Code, Start_At);
         if Op_Code = jnz then
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
      for I in Registers loop
         Put (Output_File, Indent_1);
         Put_Register (Output_File, I);
         Put_Line (Output_File, " : Integer" & Assignment & "0;");
      end loop; -- I in Registers
      New_Line (Output_File);
   end Put_Declarations;

   procedure Put_Body (Input_File, Output_File : in out File_Type) is

      Input_Line : Unbounded_String;
      First, Start_At : Positive;
      Last : Natural;
      Label_Prefix : constant String := "Label_";
      Line_Number : Line_Numbers;
      Op_Code : Op_Codes;
      Source, Register : Registers;
      Number, Immediate : Integer;
      Is_Register, Always_Register : Boolean;

      procedure Put_Label (Oitput_File : in out File_Type;
                           Line_Number : in Line_Numbers) is

      begin -- Put_Label
         Put (Output_File, Indent_1 & "<<" & Label_Prefix);
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

   begin -- Put_Body
      Put_Line (Output_File, "begin -- " & Program_Name);
      while not End_Of_File (input_File) loop
         Line_Number := Line_Numbers (Line (Input_File));
         Input_Line := Get_Line (Input_File);
         Put (Output_File, Indent_1 & "--");
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
               Put (Output_File, Indent_1);
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
               Put (Output_File, Indent_1);
               Put_Register (Output_File, Register);
               Put (Output_File, Assignment);
               Put_Register (Output_File, Register);
               if Op_Code = dec then
                  Put_Line (Output_File, " - 1;");
               elsif Op_Code = inc then
                  Put_Line (Output_File, " + 1;");
               end if; -- Op_Code = dec
            when jnz =>
               Put (Output_File, Indent_1 & "if ");
               Get_Register_or_Immediate (Input_Line, Is_Register, Register,
                                          Immediate, Start_At);
               if Is_Register then
                  Put_Register (Output_File, Register);
               else
                  Put (Output_File, Integer'Image (Immediate));
               end if; -- Is_Register
               Put_Line (Output_File, " /= 0 then");
               Put (Output_File, Indent_2);
               Put_goto (Output_File, Input_Line, Line_Number, Start_At);
               Put_Line (Output_File, Indent_1 & "end if;");
         end case; -- Op_Code
      end loop; -- not End_Of_File (input_File)
      if Last_Element (Jump_Table) > Line_Number then
         Put_Label (Output_File, Last_Element (Jump_Table));
      end if; -- Last_Element (Jump_Table) > Line_Number
      Put (Output_File, Indent_1 & "Put_Line (""");
      Put_Register (Output_File, a);
      Put (Output_File, ":"" & Integer'Image (");
      Put_Register (Output_File, a);
      Put_Line (Output_File, "));");
      Put_Line(Output_File, "end " & Program_Name & ';');
   end Put_Body;

begin -- December_12
   Open (Input_File, In_File, "20161212.txt");
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
end December_12;
