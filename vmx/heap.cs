using System;

namespace Ava
{
    internal class Heap
    {
        //
        // Arena Header format:
        // byte 0-4: arena state
        // byte 4-8: arena size
        //
        // Each arena is at most 2GB in size with
        // the arena address pointing to the first byte
        // folloring the arena header. (offset + sizeof(dword))
        //
        struct Arena
        {
            internal const int HEADER_SIZE = 3;

            internal int size;
            internal int length;
            internal State state;

            internal enum State
            {
                FREE,
                READ,
                RWRITE
            }
        }

        internal static int Alloc(Script vm, int size) {
            int address = vm.cpu.heap + Arena.HEADER_SIZE;
            // Increase size to fit header
            int nunits = size + Arena.HEADER_SIZE;

            Arena arena;

            while (address < vm.cpu.hp) {
                arena = ReadHeader(vm, address);

                // @Todo: coallescing

                if (arena.state == Arena.State.FREE && (arena.size == size || arena.size == 0)) {
                    if (address + nunits >= vm.cpu.hp) {
                        break;
                    }
                    // Found a free arena with the required size
                    arena.state = Arena.State.RWRITE;
                    arena.size  = size;
                    arena.length = size;

                    WriteHeader(vm, address, ref arena);
                    Array.Clear(vm.memory, address, size);

                    // Return pointer to first byte following the header
                    return address;
                }

                address += Arena.HEADER_SIZE + arena.size;
            }

            // No free blocks available, ask for more memory
            Resize(vm, nunits);

            if (address + nunits > vm.cpu.hp) {
                // Resize failed, memory full
                vm.cpu.status = Cpu.Status.STACK_OVERFLOW;
                return 0;
            }

            arena = new Arena() { size = size, length = size, state = Arena.State.RWRITE };
            WriteHeader(vm, address, ref arena);
            Array.Clear(vm.memory, address, size);

            return address;
        }

        internal static Cpu.Status Free(Script vm, int address) {
            if (address - sizeof(long) < vm.cpu.heap || address > vm.cpu.hp) {
                return Cpu.Status.BAD_MEMORY_ACCESS;
            }

            var arena = ReadHeader(vm, address);
            arena.state = Arena.State.FREE;

            WriteHeader(vm, address, ref arena);
            return Cpu.Status.OK;
        }

        static void Resize(Script vm, int min_size) {
            int heap_size = (vm.cpu.hp - vm.cpu.heap);
            int stack_size = (vm.memory.Length - vm.cpu.hp);
            int size_increase = Math.Max(heap_size, min_size);

            // Double heap size
            Array.Resize(ref vm.memory, vm.memory.Length + size_increase);
            // Move stack to higher address (after the new heap section)
            Array.Copy(vm.memory, vm.cpu.hp, vm.memory, vm.cpu.hp + size_increase, stack_size);
            // Clear allocated area
            Array.Clear(vm.memory, vm.cpu.hp, size_increase);
            // Increase heap size
            vm.cpu.hp += size_increase;
        }

        static Arena ReadHeader(Script vm, int address) {
            var arena = new Arena();
            arena.length = vm.ReadWord(address - 1);
            arena.size  = vm.ReadWord(address - 2);
            arena.state = (Arena.State)vm.ReadWord(address - 3);

            return arena;
        }

        static void WriteHeader(Script vm, int address, ref Arena arena) {
            vm.WriteWord(arena.length, address - 1);
            vm.WriteWord(arena.size, address - 2);
            vm.WriteWord((int)arena.state, address - 3);
        }
    }
}
