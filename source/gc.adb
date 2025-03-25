with C.gc.gc;
package body GC is
	use type C.unsigned_int;
	
	-- implementation
	
	function Version return String is
		GC_version : constant C.unsigned_int := C.gc.gc.GC_get_version;
		Major : constant C.unsigned_int := C.Shift_Right (GC_version, 16);
		Minor : constant C.unsigned_int :=
			C.Shift_Right (GC_version, 8) and (2 ** 8 - 1);
		Micro : constant C.unsigned_int := GC_version and (2 ** 8 - 1);
		Major_Image : constant String := C.unsigned_int'Image (Major);
		Minor_Image : constant String := C.unsigned_int'Image (Minor);
		Micro_Image : constant String := C.unsigned_int'Image (Micro);
	begin
		pragma Assert (Major_Image (Major_Image'First) = ' ');
		pragma Assert (Minor_Image (Minor_Image'First) = ' ');
		pragma Assert (Micro_Image (Micro_Image'First) = ' ');
		return Major_Image (Major_Image'First + 1 .. Major_Image'Last)
			& '.' & Minor_Image (Minor_Image'First + 1 .. Minor_Image'Last)
			& '.' & Micro_Image (Micro_Image'First + 1 .. Micro_Image'Last);
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
