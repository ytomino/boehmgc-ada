with Ada.Text_IO;
with GC;
procedure version is
begin
	Ada.Text_IO.Put_Line (GC.Version);
end version;
