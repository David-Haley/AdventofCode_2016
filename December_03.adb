with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Generic_Constrained_Array_Sort;

procedure December_03 is

   subtype Sides is Positive range 1 .. 3;
   subtype Side_Lengths is Positive range 1 .. 999;

   package Side_IO is new Ada.Text_IO.Integer_IO (Side_Lengths);
   use Side_IO;

   type Triangles is array (Sides) of Side_Lengths;

   procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
     (Sides, Side_Lengths, Triangles);

   subtype Columns is Positive range 1 .. 3;

   type Triangle_Groups is array (Columns) of Triangles;

   function Triangle_Count return Natural is

      Input_File : File_Type;
      Count : Natural := 0;
      Triangle : Triangles;

   begin -- Triangle_Count
      Open (Input_File, In_File, "20161203.txt");
      while not End_Of_File (Input_File) loop
         for I in Sides loop
            Get (Input_File, Triangle (I));
         end loop; --  I in Sides
         Sort (Triangle);
         if Triangle (3) < Triangle (1) + Triangle (2) then
            Count := Count + 1;
         end if; -- Triangle (3) < Triangle (1) + Triangle (2)
         Skip_Line (Input_File);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      return Count;
   end Triangle_Count;

   function Triangle_Group_Count return Natural is

      Input_File : File_Type;
      Count : Natural := 0;
      Triangle_Group : Triangle_Groups;

   begin --  Triangle_Group_Count
      Open (Input_File, In_File, "20161203.txt");
      while not End_Of_File (Input_File) loop
         for I in Sides loop
            for Column in Columns loop
               Get (Input_File, Triangle_Group (Column) (I));
            end loop; -- Column in Columns
            Skip_Line (Input_File);
            -- potentially raises exception if number of lines in file is not a
            -- multiple of three
         end loop; --  I in Sides
         for Column in Columns loop
            Sort (Triangle_Group (Column));
            if Triangle_Group (Column) (3) <
              Triangle_Group (Column) (1) + Triangle_Group (Column) (2) then
               Count := Count + 1;
            end if; -- test side lengths of one triangle
         end loop; -- Column in Columns
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      return Count;
   end  Triangle_Group_Count;

begin  -- December_03
   Put_Line ("Valid triangle count:" & Natural'image (Triangle_Count));
   Put_Line ("Valid triangle count (Part 2):" &
               Natural'image (Triangle_Group_Count));
end December_03;
