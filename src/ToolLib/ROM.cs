// ---------------------------------------------------------------------
// <copyright file="ROM.cs" company="Michael Alyn Miller">
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
// <summary>Provides a class for reading metadata from an MFORTH ROM
// file.</summary>
// ---------------------------------------------------------------------

namespace ToolLib
{
    using System.Collections.Generic;
    using System.IO;

    /// <summary>
    /// Provides an interface for reading the metadata in an MFORTH ROM
    /// file.
    /// </summary>
    public class ROM
    {
        /// <summary>
        /// Size of an MFORTH ROM.
        /// </summary>
        public const int Size = 0x8000;

        /// <summary>
        /// Address of the pointer that contains the latest (most
        /// recently-defined) word in the MFORTH ROM.
        /// </summary>
        private const int LatestWordPtrAddress = 0x7FFE;

        /// <summary>
        /// Raw bytes contained in the ROM file.
        /// </summary>
        private byte[] romBytes;

        /// <summary>
        /// List of words in the dictionary.
        /// </summary>
        private IDictionary<string, Word> wordList;

        /// <summary>
        /// Initializes a new instance of the ROM class by reading the
        /// given ROM into memory.
        /// </summary>
        /// <param name="romPath">Path to the MFORTH ROM.</param>
        public ROM(string romPath)
        {
            // Open the ROM and read it into memory.
            this.romBytes = new byte[32768];
            using (var f = File.Open(romPath, FileMode.Open, FileAccess.Read))
            {
                int bytesRead = f.Read(this.romBytes, 0, this.romBytes.Length);
                if (bytesRead != this.romBytes.Length)
                {
                    throw new InvalidDataException(
                        string.Format(
                            "MFORTH ROM was only {0} bytes long; expected {1} bytes.",
                            bytesRead,
                            this.romBytes.Length));
                }
            }

            // Load the word list into memory.  Duplicate word names are
            // not allowed.
            this.wordList = new Dictionary<string, Word>();
            for (int curWord = this.LatestWordAddr; curWord != 0;)
            {
                // Create the word, check for duplicate names, then
                // store the word in our list.
                var word = new Word(this, curWord);
                System.Diagnostics.Debug.Assert(
                    !this.wordList.ContainsKey(word.Name),
                    "Duplicate word name found in dictionary.");
                this.wordList[word.Name] = word;

                // Move to the next (previous) word.
                curWord = word.NextWordAddr;
            }
        }

        /// <summary>
        /// Gets the address of the latest word defined in the ROM.
        /// </summary>
        public int LatestWordAddr
        {
            get
            {
                return this.GetInt16(LatestWordPtrAddress);
            }
        }

        /// <summary>
        /// Gets the number of words defined in the ROM.
        /// </summary>
        public int NumWords
        {
            get
            {
                return this.wordList.Count;
            }
        }

        /// <summary>
        /// Gets the words defined in the MFORTH ROM.
        /// </summary>
        public IEnumerable<Word> Words
        {
            get
            {
                return this.wordList.Values;
            }
        }

        /// <summary>
        /// Returns the 8-bit integer stored at the given address in the
        /// ROM.
        /// </summary>
        /// <param name="address">Address of the integer.</param>
        /// <returns>The integer stored at address.</returns>
        public byte GetInt8(int address)
        {
            return this.romBytes[address];
        }

        /// <summary>
        /// Returns the 16-bit integer stored at the given address in
        /// the ROM.
        /// </summary>
        /// <param name="address">Address of the integer.</param>
        /// <returns>The integer stored at address.</returns>
        public int GetInt16(int address)
        {
            return this.GetInt8(address) | (this.GetInt8(address + 1) << 8);
        }
    }
}
