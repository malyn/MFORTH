// ---------------------------------------------------------------------
// <copyright file="PearsonHashFunction.cs" company="Michael Alyn Miller">
// Copyright (c) 2009-2010, Michael Alyn Miller (malyn@strangeGizmo.com).
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// 1. Redistributions of source code must retain the above copyright
//    notice unmodified, this list of conditions, and the following
//    disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in
//    the documentation and/or other materials provided with the
//    distribution.
// 3. Neither the name of Michael Alyn Miller nor the names of the
//    contributors to this software may be used to endorse or promote
//    products derived from this software without specific prior written
//    permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
// BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
// OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
// OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// </copyright>
// <summary>Provides an implemenation of Peter K. Pearson's hash
// function.</summary>
// ---------------------------------------------------------------------

namespace ToolLib
{
    using System;
    using System.Collections.Generic;

    /// <summary>
    /// Hash function that implements Peter K. Pearson's mechanism as
    /// described in his "Fast Hashing of Variable-Length Text Strings"
    /// paper.
    /// </summary>
    public class PearsonHashFunction
    {
        /// <summary>
        /// Size in bytes of a Pearson Hash Function auxilliary table.
        /// </summary>
        public const int AuxilliaryTableSize = 256;

        /// <summary>
        /// Auxilliary table for the Pearson Hash Function.
        /// </summary>
        private byte[] auxTable;

        /// <summary>
        /// Initializes a new instance of the PearsonHashFunction class
        /// with a random auxilliary table.
        /// </summary>
        /// <param name="random">Random number generator used to shuffle
        /// the auxilliary table.</param>
        public PearsonHashFunction(Random random)
        {
            // Generate the initial auxilliary table.
            this.auxTable = new byte[AuxilliaryTableSize];
            for (int i = 0; i < this.auxTable.Length; i++)
            {
                this.auxTable[i] = (byte)i;
            }

            // Shuffle the auxilliary table.
            this.ShuffleAuxilliaryTable(random);
        }

        /// <summary>
        /// Gets the auxilliary table for this PearsonHash object.
        /// </summary>
        public IEnumerable<byte> AuxilliaryTable
        {
            get
            {
                return this.auxTable;
            }
        }

        /// <summary>
        /// Hash a buffer.
        /// </summary>
        /// <param name="buf">The buffer to hash.</param>
        /// <returns>The hash value for the string use the auxilliary
        /// table for this PearsonHash object.</returns>
        public byte Hash(byte[] buf)
        {
            byte hash = 0;
            foreach (byte b in buf)
            {
                hash = this.auxTable[hash ^ b];
            }

            return hash;
        }

        /// <summary>
        /// Shuffle the auxilliary table.
        /// </summary>
        /// <param name="random">Random number generator used to shuffle
        /// the table.</param>
        public void ShuffleAuxilliaryTable(Random random)
        {
            FisherYatesShuffle<byte>(this.auxTable, random);
        }

        /// <summary>
        /// Shuffle the given array use the Fisher-Yates method.
        /// </summary>
        /// <typeparam name="TValue">Type of the values in the
        /// array.</typeparam>
        /// <param name="array">The array to be shuffled.</param>
        /// <param name="random">Random number generator used to shuffle
        /// the array.</param>
        private static void FisherYatesShuffle<TValue>(TValue[] array, Random random)
        {
            // Shuffle the array.
            for (int i = array.Length; i > 1; i--)
            {
                // Pick a random element to swap with the i-th element.
                int j = random.Next(i); // 0 <= j <= i-1 (0-based array)

                // Swap array elements.
                TValue tmp = array[j];
                array[j] = array[i - 1];
                array[i - 1] = tmp;
            }
        }
    }
}
