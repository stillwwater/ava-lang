using System;

namespace Ava
{
    class Float32
    {
        internal static float FromInt(int value) {
            return BitConverter.ToSingle(BitConverter.GetBytes(value), 0);
        }

        internal static int ToInt(float value) {
            return BitConverter.ToInt32(BitConverter.GetBytes(value), 0);
        }
    }
}
