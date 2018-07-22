with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;

procedure December_15 is

   subtype Cycles is Natural range 1 .. Natural'Last;
   subtype Positions is Natural;

   type Discs is record
      Cycle : Cycles;
      Starting : Positions;
   end record; -- Discs

   subtype Disc_Numbers is Positive;

   package Disc_Vectors is new Ada.Containers.Vectors (Disc_Numbers, Discs);
   use Disc_Vectors;

   Disc_Vector : Disc_Vectors.Vector := Empty_Vector;

   subtype Model_Ticks is Natural;
   Model_Tick : Model_Ticks := 0;
   Found : Boolean;


   procedure Read_Input (Disc_Vector : in out Disc_Vectors.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Disc_Index : Disc_Numbers;
      Disc : Discs;
      Disc_String : constant String := "Disc #";
      Cycle_String : constant String := "has";
      Position_String : constant String := "it is at position";

   begin -- Read_Input
      Open (Input_File, In_File, "20161215.txt");
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Start_At := Disc_String'Length + Index (Text, Disc_String, Start_At);
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Disc_index := Disc_Numbers'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Start_At := Cycle_String'Length + Index (Text, Cycle_String, Start_At);
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Disc.Cycle := Cycles'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Start_At := Position_String'Length + Index (Text, Position_String,
                                                     Start_At);
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Disc.Starting := Positions'Value (Slice (Text, First, Last));
         Append (Disc_Vector, Disc);
         Assert (Disc_Index = Last_Index (Disc_Vector), "Append Failed");
      end loop; --not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Current_Position (Disc_Vector : in Disc_Vectors.Vector;
                              Disc_Index : in Disc_Numbers;
                              Model_Tick : in Model_Ticks) return Positions is

      -- Returns the position that the disc will be in when it ie reached by
      -- the capsule. Since the time between discs is one tick adding the
      -- Disc_Index to the Model_Tick calculates the time the capsule will
      -- reach a specific disc if it is released at time Model_Tick.

   begin -- Current_Position
      return (Constant_Reference (Disc_Vector, Disc_Index).Starting +
                Disc_Index + Model_Tick) mod
          Constant_Reference (Disc_Vector, Disc_Index).Cycle;
   end Current_Position;

begin -- December_15
   Read_Input (Disc_Vector);
   while Current_Position (Disc_Vector, 1, Model_Tick) /= 0 loop
      Model_Tick := Model_Tick + 1;
   end loop; -- Current_Position (Disc_Vector, 1, Model_Tick) /= 0
   loop -- find pass through
      Found := True;
      for I in Disc_Numbers range First_Index (Disc_Vector) ..
        Last_Index (Disc_Vector) loop
         Found := Found and Current_Position (Disc_Vector, I, Model_Tick) = 0;
      end loop; --in Disc_Numbers range First_Index (Disc_Vector) ..
      exit when Found;
      Model_Tick := Model_Tick + Constant_Reference (Disc_Vector, 1).Cycle;
      -- adds the time required for first disc to return to 0 position
   end loop; -- find pass through
   Put_Line ("Release at:" & Model_Ticks'Image (Model_Tick));
   -- Part two 11 position disc added
   Append (Disc_Vector, (11, 0));
   Model_Tick := 0;
   while Current_Position (Disc_Vector, 1, Model_Tick) /= 0 loop
      Model_Tick := Model_Tick + 1;
   end loop; -- Current_Position (Disc_Vector, 1, Model_Tick) /= 0
   loop -- find pass through
      Found := True;
      for I in Disc_Numbers range First_Index (Disc_Vector) ..
        Last_Index (Disc_Vector) loop
         Found := Found and Current_Position (Disc_Vector, I, Model_Tick) = 0;
      end loop; --in Disc_Numbers range First_Index (Disc_Vector) ..
      exit when Found;
      Model_Tick := Model_Tick + Constant_Reference (Disc_Vector, 1).Cycle;
      -- adds the time required for first disc to return to 0 position
   end loop; -- find pass through
   Put_Line ("Part two release at:" & Model_Ticks'Image (Model_Tick));
end December_15;
