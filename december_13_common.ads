with Interfaces; use Interfaces;

package December_13_Common is

   subtype Coordinates is Unsigned_32 range 0 .. 1000;
   type Bread_Crumbs is array (Coordinates, Coordinates) of Boolean
     with Pack => True;

   Bread_Crumb : Bread_Crumbs := (others => (others => False));

   function Is_Open_Space (X, Y : in Coordinates) return Boolean;

end December_13_Common;
