with C.gc.gc;
package body GC is
	
	function Heap_Size return System.Storage_Elements.Storage_Count is
	begin
		return System.Storage_Elements.Storage_Count (C.gc.gc.GC_get_heap_size);
	end Heap_Size;
	
	procedure Collect is
	begin
		C.gc.gc.GC_gcollect;
	end Collect;
	
end GC;
