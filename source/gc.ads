with System.Storage_Elements;
package GC is
	pragma Preelaborate;
	pragma Linker_Options ("-lgc");
	
	function Heap_Size return System.Storage_Elements.Storage_Count;
	
	procedure Collect;
	
end GC;
