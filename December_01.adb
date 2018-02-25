with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_01 is

   subtype Coordinates is Integer range -1000 .. 1000;

   type Locations is record
      X, Y : Coordinates;
   end record; -- Locations

   Grid : array (Coordinates, Coordinates) of Boolean :=
     (others => (others => False));

   Location : Locations := (0, 0);
   Easter_Bunny_HQ : Locations;
   HQ_Found : Boolean := False;


   subtype Steps is Positive;
   Step : Steps;
   package Step_IO is new Ada.Text_IO.Integer_IO (Steps);
   use Step_IO;

   type Turns is (L, R);
   Turn_Set : Character_Set := To_Set ("LR");
   Turn : Turns;
   package Turn_IO is new Ada.Text_IO.Enumeration_IO (Turns);
   use Turn_IO;

   type Directions is (North, East, South, West);

   Direction : Directions := North;

   type Direction_Tables is array (Directions, Turns) of Directions;

   New_Direction : constant Direction_Tables :=
     (North => (L => West,  R => East),
      East  => (L => North, R => South),
      South => (L => East,  R => West),
      West  => (L => South, R => North));

   Input_File : File_Type;
   Input_Line : Unbounded_String;
   Start_At, First : Positive;
   Last : Natural;

begin  -- December_01
   Open (Input_File, In_File, "20161201.txt");
   Input_Line := To_Unbounded_String (Get_Line (Input_File));
   Close (Input_File);
   Start_At := 1;
   Grid (0, 0) := True;
   while Start_At <= Length (Input_Line) loop
      Find_Token (Input_Line, Turn_Set, Start_At, Inside, First, Last);
      Get (Slice (Input_Line, First, Last), Turn, Start_At);
      Start_At := Start_At + 1;
      Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Get (Slice (Input_Line, First, Last), Step, Start_At);
      Start_At := Start_At + 1;
      Direction := New_Direction (Direction, Turn);
      case Direction is
         when North =>
            for Y in Coordinates range Location.Y + 1 .. Location.Y + Step loop
               if Grid (Location.X, Y) and not HQ_Found then
                  Easter_Bunny_HQ := (Location.X, Y);
                  HQ_Found := True;
               end if; -- Grid (Location.X, Y) and not HQ_Found
               Grid (Location.X, Y) := True;
            end loop; -- mark path going North
            Location.Y := Location.Y + Step;
         When East =>
            for X in Coordinates range Location.X + 1 .. Location.X + Step loop
               if Grid (X, Location.Y) and not HQ_Found then
                  Easter_Bunny_HQ := (X, Location.Y);
                  HQ_Found := True;
               end if; -- Grid (Location.X, Y) and not HQ_Found
               Grid (X, Location.Y) := True;
            end loop; -- mark path going East
            Location.X := Location.X + Step;
         when South =>
            for Y in reverse Coordinates range Location.Y - Step ..
              Location.Y - 1 loop
               if Grid (Location.X, Y) and not HQ_Found then
                  Easter_Bunny_HQ := (Location.X, Y);
                  HQ_Found := True;
               end if; -- Grid (Location.X, Y) and not HQ_Found
               Grid (Location.X, Y) := True;
            end loop; -- mark path going South
            Location.Y := Location.Y - Step;
         when West =>
            for X in reverse Coordinates range Location.X - Step ..
              Location.X - 1 loop
               if Grid (X, Location.Y) and not HQ_Found then
                  Easter_Bunny_HQ := (X, Location.Y);
                  HQ_Found := True;
               end if; -- Grid (Location.X, Y) and not HQ_Found
               Grid (X, Location.Y) := True;
            end loop; -- mark X path
            Location.X := Location.X - Step;
      end case; -- Direction
   end loop; -- End_Of_File (Input_File)
   Put_Line ("Distance:" &
               Coordinates'Image (abs (Location.X) + abs (Location.Y)));
   Put_Line ("Easter Bunny HQ distance (Part Two)" &
               Coordinates'Image (abs (Easter_Bunny_HQ.X) +
               abs (Easter_Bunny_HQ.Y)));
end December_01;
