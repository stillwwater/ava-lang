using System;

namespace Ava
{
    internal class Heap
    {
        struct Arena
        {
            internal uint size;
            internal State state;

            internal enum State
            {
                FREE,
                READ,
                RWRITE
            }
        }

        internal static uint Alloc(Script vm, uint size) {
            uint address = vm.cpu.heap + sizeof(long);
            // Increase size to fit header
            uint nunits = size + sizeof(long);

            Arena arena;

            while (address + nunits < vm.cpu.hp) {
                arena = ReadHeader(vm, address);

                // @Todo: coallescing

                if (arena.state == Arena.State.FREE && (arena.size == size || arena.size == 0)) {
                    // Found a free arena with the required size
                    arena.state = Arena.State.RWRITE;
                    arena.size = size;
                    WriteHeader(vm, address, ref arena);

                    // Return pointer to first byte following the header
                    return address;
                }

                address += arena.size + sizeof(long);
            }

            // No free blocks available, ask for more memory
            Resize(vm, nunits);

            if (address + nunits > vm.cpu.hp) {
                // Resize failed, memory full
                vm.cpu.status.error = Cpu.Error.STACK_OVERFLOW;
                return 0;
            }

            arena = new Arena() { size = size, state = Arena.State.RWRITE };
            WriteHeader(vm, address, ref arena);

            return address;
        }

        internal static bool Free(Script vm, uint address) {
            if (address - sizeof(long) < vm.cpu.heap || address > vm.cpu.hp) {
                vm.cpu.status.error = Cpu.Error.BAD_MEMORY_ACCESS;
                return true;
            }

            var arena = ReadHeader(vm, address);
            arena.state = Arena.State.FREE;

            WriteHeader(vm, address, ref arena);
            return false;
        }

        static void Resize(Script vm, uint min_size) {
            uint heap_size = (vm.cpu.hp - vm.cpu.heap);
            uint stack_size = (uint)(vm.memory.Length - vm.cpu.hp);
            int size_increase = (int)Math.Max(heap_size, min_size);

            // Double heap size
            Array.Resize(ref vm.memory, vm.memory.Length + size_increase);
            // Move stack to higher address (after the new heap section)
            Array.Copy(vm.memory, (int)vm.cpu.hp, vm.memory, (int)vm.cpu.hp + size_increase, (int)stack_size);
            // Clear allocated area
            Array.Clear(vm.memory, (int)vm.cpu.hp, size_increase);
            // Increase heap size
            vm.cpu.hp += (uint)size_increase;
        }

        //
        // Header format:
        // byte 0: arena state
        // byte 1-4: arena size (24 bit value)
        //
        // Each arena is at most 16MB in size with
        // the arena address pointing to the first byte
        // folloring the arena. (offset + sizeof(word))
        //
        static Arena ReadHeader(Script vm, uint address) {
            var arena  = new Arena();
            arena.size = (uint)vm.ReadWord(address - 4);
            arena.state = (Arena.State)vm.ReadWord(address - 8);

            //arena.size   = data & 0x00FFFFFF;
            //arena.state = (Arena.State)((data & 0xFF000000) >> 24);

            return arena;
        }

        static void WriteHeader(Script vm, uint address, ref Arena arena) {
            //uint data = arena.size | (uint)arena.state << 24;
            vm.WriteWord(arena.size, address - 4);
            vm.WriteWord((uint)arena.state, address - 8);
        }
    }
}
