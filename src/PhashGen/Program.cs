// ---------------------------------------------------------------------
// <copyright file="Program.cs" company="Michael Alyn Miller">
// Copyright (c) 2009-2011, Michael Alyn Miller (malyn@strangeGizmo.com).
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
// <summary>Command line utility for generating an MFORTH hash
// table.</summary>
// ---------------------------------------------------------------------

namespace PhashGen
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Text;
    using ToolLib;

    /// <summary>
    /// Command line utility for generating an MFORTH hash table.
    /// </summary>
    public class Program
    {
        /// <summary>
        /// The seed value to use for the random number generator.  This
        /// value is updated as new words are added to MFORTH in order
        /// to ensure that the hash table is generated quickly and with
        /// most words at their first hash location (which provides a
        /// marginal increase to lookup performance in MFORTH).
        /// </summary>
        private static int HashTableRandomSeed = 135960;

        /// <summary>
        /// The size of the hash table; this value needs to be a power
        /// of two and needs to be large enough to include all of the
        /// words in the MFORTH ROM without causing a high number of
        /// hash collisions.
        /// </summary>
        private const int HashTableSize = 1024;

        /// <summary>
        /// The mask value used to reduce the 16-bit hash value down to
        /// a value that is appropriate for the size of the hash table.
        /// </summary>
        private const int HashMask = HashTableSize - 1;

        /// <summary>
        /// phashgen entry point.
        /// </summary>
        /// <param name="args">ROM path, symbol table path, output
        /// path.</param>
        public static void Main(string[] args)
        {
            // Load the ROM.
            var rom = new ROM(args[0]);

            // Load the symbol table.
            IDictionary<int, string> symbolTable = LoadSymbolTable(args[1]);

            // Open the output file.
            StreamWriter outFile = new StreamWriter(args[2], false, Encoding.ASCII);

            // Generate the hash table.
            Console.Write("Generating PHASH tables: ");
            PearsonHashFunction phf1, phf2;
            var hashTable = GenerateHashTable(rom, out phf1, out phf2);
            Console.WriteLine(" Done!");
            Console.WriteLine(
                "Total words: {0}; at first hash location: {1}; at second hash location: {2}",
                rom.NumWords,
                hashTable.NumValuesStoredAtFirstHash,
                hashTable.NumValuesStoredAtSecondHash);

            // Write out the mask value and auxilliary tables used to
            // construct the hash table.
            outFile.WriteLine(
                "PHASHMASK:  .EQU    0{0:X2}h",
                HashMask >> 8);

            outFile.WriteLine(
                "PHASHAUX1:  .ORG    0{0:X4}h",
                ROM.Size - (HashTableSize << 1) - (2 * PearsonHashFunction.AuxilliaryTableSize));
            WriteByteData(outFile, phf1.AuxilliaryTable);

            outFile.WriteLine(
                "PHASHAUX2:  .ORG    0{0:X4}h",
                ROM.Size - (HashTableSize << 1) - PearsonHashFunction.AuxilliaryTableSize);
            WriteByteData(outFile, phf2.AuxilliaryTable);

            // Write out the hash table.
            outFile.WriteLine(
                "PHASHTAB:   .ORG    0{0:X4}h",
                ROM.Size - (HashTableSize << 1));
            WriteHashTable(outFile, hashTable, symbolTable);

            // Close the output file; we're done!
            outFile.Close();
        }

        #region Generate and Write Hash Table

        /// <summary>
        /// Hash the given name using the given hash functions.
        /// </summary>
        /// <param name="name">The name to hash.</param>
        /// <param name="phf1">PearsonHashFunction to use for the upper
        /// eight bits of the hash.</param>
        /// <param name="phf2">PearsonHashFunction to use for the lower
        /// eight bits of the hash.</param>
        /// <param name="hashMask">The mask value to be applied to the
        /// hash.</param>
        /// <returns>The hash value, masked by hashMask.</returns>
        private static int HashName(string name, PearsonHashFunction phf1, PearsonHashFunction phf2, int hashMask)
        {
            // Get the word name as an array of uppercase ASCII bytes.
            string upperName = name.ToUpperInvariant();
            byte[] upperNameBytes = Encoding.ASCII.GetBytes(upperName);

            // Hash the name.  Use phf1 for the upper eight bits and
            // phf2 for the lower eight bits.  Then mask off the hash
            // value to keep it within range of our hash table.
            return ((phf1.Hash(upperNameBytes) << 8) | phf2.Hash(upperNameBytes)) & hashMask;
        }

        /// <summary>
        /// Generate the MFORTH hash table.
        /// </summary>
        /// <param name="rom">MFORTH ROM.</param>
        /// <param name="phf1">Upon return, will contain the first
        /// PearsonHashFunction used by the hash table.</param>
        /// <param name="phf2">Upon return, will contain the second
        /// PearsonHashFunction used by the hash table.</param>
        /// <returns>The MFORTH hash table.</returns>
        private static CuckooHashTable<Word> GenerateHashTable(ROM rom, out PearsonHashFunction phf1, out PearsonHashFunction phf2)
        {
            // Initialize our random number generator.
            var random = new Random(HashTableRandomSeed);

            // Initialize our hash functions and the hash table.
            var hashFunc1 = phf1 = new PearsonHashFunction(random);
            var hashFunc2 = phf2 = new PearsonHashFunction(random);
            var hashTable = new CuckooHashTable<Word>(
                (w) => HashName(w.Name, hashFunc1, hashFunc2, HashMask),
                (w) => HashName(w.Name, hashFunc2, hashFunc1, HashMask));

            // Generate the hash table.
#if FINDING_OPTIMAL_HASH_SEED
            int minValuesAtSecondLocation = int.MaxValue;
#endif
            for (;;)
            {
#if !FINDING_OPTIMAL_HASH_SEED
                // Display a progress dot.
                Console.Write(".");
#endif

                // Add all of the words to our hash table; assume that
                // we will be successful.
                bool haveCompleteTable = true;
                foreach (var word in rom.Words)
                {
                    if (!hashTable.TryAddValue(word))
                    {
                        haveCompleteTable = false;
                        break;
                    }
                }

#if !FINDING_OPTIMAL_HASH_SEED
                // We're done if all of the words were added to the
                // table.
                if (haveCompleteTable)
                {
                    break;
                }

                // We failed to generate a complete hash table; shuffle
                // the hash functions, clear the table, and try again.
                phf1.ShuffleAuxilliaryTable(random);
                phf2.ShuffleAuxilliaryTable(random);
                hashTable.Clear();
#else
                // Print out the results if this seed produced better
                // results than the previous best seed.
                if (haveCompleteTable
                    && hashTable.NumValuesStoredAtSecondHash < minValuesAtSecondLocation)
                {
                    Console.WriteLine(
                        "Seed: {0}; 1st: {1}; 2nd: {2}",
                        HashTableRandomSeed,
                        hashTable.NumValuesStoredAtFirstHash,
                        hashTable.NumValuesStoredAtSecondHash);
                    minValuesAtSecondLocation = hashTable.NumValuesStoredAtSecondHash;
                }

                // Generate a new seed and re-create all of the hash
                // functions and tables.
                random = new Random(++HashTableRandomSeed);
                hashFunc1 = phf1 = new PearsonHashFunction(random);
                hashFunc2 = phf2 = new PearsonHashFunction(random);
                hashTable = new CuckooHashTable<Word>(
                    (w) => HashName(w.Name, hashFunc1, hashFunc2, HashMask),
                    (w) => HashName(w.Name, hashFunc2, hashFunc1, HashMask));
#endif
            }

            // Ensure that all of the words in the ROM were added to the
            // hash table.
            if (rom.NumWords != hashTable.Count)
            {
                throw new InvalidDataException(
                    string.Format(
                        "Hash table only has {0} words; expected {1} words.",
                        hashTable.Count,
                        rom.NumWords));
            }

            // Return the hash table.
            return hashTable;
        }

        /// <summary>
        /// Write the MFORTH hash table to the given writer.
        /// </summary>
        /// <param name="writer">The writer where the hash table should be
        /// written.</param>
        /// <param name="hashTable">MFORTH hash table.</param>
        /// <param name="symbolTable">TASM symbol table for the MFORTH
        /// ROM.</param>
        private static void WriteHashTable(TextWriter writer, CuckooHashTable<Word> hashTable, IDictionary<int, string> symbolTable)
        {
            // Write out every possible entry in the hash table; not all
            // hash locations will be filled.
            for (int i = 0; i < HashTableSize; i++)
            {
                // Try to retrieve the word at this hash location; write
                // out a zero value if no word is stored at this
                // location.
                Word word;
                if (!hashTable.TryGetValue(i, out word))
                {
                    writer.WriteLine("            .WORD   0");
                    continue;
                }

                // There is a word stored at this hash location.  Find
                // symbol associated with this word.  Symbols point to
                // the CFA of each word, so we need to offset the word
                // address by the NFATOCFASZ.  That value is different
                // between standard MFORTH builds and profiled MFORTH
                // builds.  We try the standard value first and then
                // fall back to the profile value.
                string label;
                if (!symbolTable.TryGetValue(word.NFA + 3, out label))
                {
                    label = symbolTable[word.NFA + 5];
                }

                // Write out the hash entry, which must point to the NFA
                // of the word and not the CFA.
                writer.WriteLine("            .WORD   {0}-NFATOCFASZ", label);
            }
        }

        #endregion

        #region TASM Utils

        /// <summary>
        /// Load a TASM symbol table into a dictionary.  Only those
        /// symbols which refer to MFORTH words will be returned.
        /// </summary>
        /// <param name="symbolTablePath">Path to the TASM symbol
        /// table.</param>
        /// <returns>The symbol table, indexed by symbol
        /// address.</returns>
        private static IDictionary<int, string> LoadSymbolTable(string symbolTablePath)
        {
            // Load the symbol table into memory.
            using (var f = new StreamReader(symbolTablePath, Encoding.ASCII))
            {
                // Create an empty symbol table, then load all of the
                // values from the symbol file.
                var symbolTable = new Dictionary<int, string>();
                while (!f.EndOfStream)
                {
                    // Read the line from the symbol table, then split
                    // the entry into two parts.
                    string line = f.ReadLine();
                    string[] entry = line.Split(new char[] { ' ' }, 2);
                    string symbolName = entry[0].Trim();
                    int symbolAddress = Int32.Parse(entry[1], System.Globalization.NumberStyles.HexNumber);

                    // Ignore entries that start with "noname." (those
                    // are internal symbols used inside of a definition)
                    // as well as entries that start with "LINK_" and
                    // "LAST_", which are used to chain dictionary
                    // entries together across assembler files.
                    if (symbolName.StartsWith("noname.")
                        || symbolName.StartsWith("LINK_")
                        || symbolName.StartsWith("LAST_"))
                    {
                        continue;
                    }

                    // Add the symbol to our table.
                    symbolTable[symbolAddress] = symbolName;
                }

                // Return the symbol table.
                return symbolTable;
            }
        }

        /// <summary>
        /// Write byte data to the given writer.
        /// </summary>
        /// <param name="writer">The writer where the bytes should be
        /// written.</param>
        /// <param name="bytes">The bytes to be written.</param>
        private static void WriteByteData(TextWriter writer, IEnumerable<byte> bytes)
        {
            // Write out all of the bytes.
            int pos = 0;
            foreach (byte b in bytes)
            {
                // Begin a new line after every eight bytes.
                if (pos++ % 8 == 0)
                {
                    // Finish the previous line; don't do this for the
                    // very first line though.
                    if (pos != 1)
                    {
                        writer.WriteLine();
                    }

                    // Begin the new data line.
                    writer.Write("            .BYTE   ");
                }

                // Output this byte.
                writer.Write("{0},", b);
            }

            // Finish the last line.
            writer.WriteLine();
        }

        #endregion
    }
}
