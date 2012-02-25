// ---------------------------------------------------------------------
// <copyright file="Word.cs" company="Michael Alyn Miller">
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
// <summary>Provides a class that represents a single MFORTH word
// defined in the ROM file.</summary>
// ---------------------------------------------------------------------

namespace ToolLib
{
    using System.IO;
    using System.Text;

    /// <summary>
    /// Represents a single MFORTH word defined in the ROM file.
    /// </summary>
    public class Word
    {
        /// <summary>
        /// Offset from the NFA to the LFA.
        /// </summary>
        private const int NFATOLFASZ = 1;

        /// <summary>
        /// Initializes a new instance of the Word class whose Name
        /// Field Address is located at "nfa" in the given ROM.
        /// </summary>
        /// <param name="rom">MFORTH ROM.</param>
        /// <param name="nfa">Name Field Address of the word.</param>
        public Word(ROM rom, int nfa)
        {
            // Store the NFA.
            this.NFA = nfa;

            // Get the name of the word.
            this.Name = GetWordName(rom, nfa);

            // Get the address of the next (previous) word in the
            // dictionary.
            this.NextWordAddr = rom.GetInt16(nfa + NFATOLFASZ);
        }

        /// <summary>
        /// Gets the name of the word.
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// Gets the Name Field Address of this word.
        /// </summary>
        public int NFA { get; private set; }

        /// <summary>
        /// Gets the address of the next word, or zero if this is the
        /// oldest word in the dictionary.
        /// </summary>
        public int NextWordAddr { get; private set; }

        /// <summary>
        /// Gets the name of the word whose NFA is at the given address.
        /// </summary>
        /// <param name="rom">MFORTH ROM.</param>
        /// <param name="nfa">Name Field Address of the start of the
        /// name.</param>
        /// <returns>The name of the word.</returns>
        private static string GetWordName(ROM rom, int nfa)
        {
            // Get the length of the word's name from the byte at the
            // NFA.
            int wordLen = rom.GetInt8(nfa) & 0x3f;

            // Read the bytes in the word's name.  Ignore the length for
            // now and just use the end-of-name flag to determine when
            // to stop reading bytes.
            var word = new StringBuilder();
            for (int romOffset = nfa - 1;; romOffset--)
            {
                byte c = rom.GetInt8(romOffset);
                word.Append(Encoding.ASCII.GetString(new byte[] { (byte)(c & 0x7F) }));
                if ((c & 0x80) != 0)
                {
                    break;
                }
            }

            // Ensure that the decoded name is the correct length.
            if (word.Length != wordLen)
            {
                throw new InvalidDataException(
                    string.Format(
                        "Word '{0}' has NFA with incorrect length {1}.",
                        word,
                        wordLen));
            }

            // Return the name.
            return word.ToString();
        }
    }
}
