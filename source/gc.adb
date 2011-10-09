with C.gc.gc;
package body GC is
	use type C.unsigned_int;
	
	-- the version variable is not declared in header files
	GC_version : C.unsigned_int;
	pragma Import (C, GC_version, "GC_version");
	
	-- implementation
	
	function Version return String is
		Major : constant C.unsigned_int := C.Shift_Right (GC_version, 16);
		Minor : constant C.unsigned_int :=
			C.Shift_Right (GC_version, 8) and (2 ** 8 - 1);
		Alpha : constant C.unsigned_int := GC_version and (2 ** 8 - 1);
		Major_Image : constant String := C.unsigned_int'Image (Major);
		Minor_Image : constant String := C.unsigned_int'Image (Minor);
	begin
		pragma Assert (Major_Image (Major_Image'First) = ' ');
		pragma Assert (Minor_Image (Minor_Image'First) = ' ');
		if Alpha /= 255 then
			declare
				Alpha_Image : constant String := C.unsigned_int'Image (Alpha);
			begin
				return Major_Image (Major_Image'First + 1 .. Major_Image'Last)
					& '.' & Minor_Image (Minor_Image'First + 1 .. Minor_Image'Last)
					& "alpha" & Alpha_Image (Alpha_Image'First + 1 .. Alpha_Image'Last);
			end;
		else
			return Major_Image (Major_Image'First + 1 .. Major_Image'Last)
				& '.' & Minor_Image (Minor_Image'First + 1 .. Minor_Image'Last);
		end if;
	end Version;
	
	function Heap_Size return System.Storage_Elements.Storage_Count is
	begin
		return System.Storage_Elements.Storage_Count (C.gc.gc.GC_get_heap_size);
	end Heap_Size;
	
	procedure Collect is
	begin
		C.gc.gc.GC_gcollect;
	end Collect;
	
end GC;
