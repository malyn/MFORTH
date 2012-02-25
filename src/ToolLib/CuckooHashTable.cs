// ---------------------------------------------------------------------
// <copyright file="CuckooHashTable.cs" company="Michael Alyn Miller">
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
// <summary>Provides a hash table implemented with a Cuckoo Hash.</summary>
// ---------------------------------------------------------------------

namespace ToolLib
{
    using System.Collections.Generic;

    /// <summary>
    /// Hash table implemented using a Cuckoo Hash.
    /// </summary>
    /// <typeparam name="TValue">Type of values stored in the hash
    /// table.</typeparam>
    public class CuckooHashTable<TValue>
    {
        /// <summary>
        /// First hash function for use by the Cuckoo Hash.
        /// </summary>
        private CuckooHashFunction chf1;

        /// <summary>
        /// First hash function for use by the Cuckoo Hash.
        /// </summary>
        private CuckooHashFunction chf2;

        /// <summary>
        /// The hash table.
        /// </summary>
        private IDictionary<int, CuckooHashEntry> hashTable = new Dictionary<int, CuckooHashEntry>();

        /// <summary>
        /// Initializes a new instance of the CuckooHashTable class.
        /// </summary>
        /// <param name="chf1">First hash function for use by the Cuckoo
        /// Hash.</param>
        /// <param name="chf2">Second hash function for use by the
        /// Cuckoo Hash.</param>
        public CuckooHashTable(CuckooHashFunction chf1, CuckooHashFunction chf2)
        {
            this.chf1 = chf1;
            this.chf2 = chf2;
        }

        /// <summary>
        /// Delegate which hashes the given value into an integer.
        /// </summary>
        /// <param name="value">The value to be hashed.</param>
        /// <returns>An integer hash value.</returns>
        public delegate int CuckooHashFunction(TValue value);

        /// <summary>
        /// Gets the number of entries in the hash table.
        /// </summary>
        public int Count
        {
            get
            {
                return this.hashTable.Count;
            }
        }

        /// <summary>
        /// Gets the number of hash values stored at their first hash
        /// location.
        /// </summary>
        public int NumValuesStoredAtFirstHash { get; private set; }

        /// <summary>
        /// Gets number of hash values stored at their second hash
        /// location.
        /// </summary>
        public int NumValuesStoredAtSecondHash { get; private set; }

        /// <summary>
        /// Try to add a value to the hash table.  The hash functions
        /// must be rebuilt if the add operation fails (and assuming
        /// that you want to be able to add this value to the table).
        /// The table must be cleared after rebuilding the hash
        /// functions if the same CuckooHashTable object will be reused.
        /// </summary>
        /// <param name="value">The value to add to the hash
        /// table.</param>
        /// <returns>true if the value was added to the table; false if
        /// trying to add the value caused an infinite loop.</returns>
        public bool TryAddValue(TValue value)
        {
            // Wrap the value in a CuckooHashEntry.
            var hashEntry = new CuckooHashEntry(this.chf1(value), this.chf2(value), value);

            // Try to put the value in its first position.
            if (!this.hashTable.ContainsKey(hashEntry.Hash1))
            {
                this.hashTable[hashEntry.Hash1] = hashEntry;
                this.NumValuesStoredAtFirstHash++;
            }
            else
            {
                // We need to kick the entry in our Hash1 position into
                // its Hash2 position.  This could cause other entries
                // to be kicked out of position, which could be an
                // infinite loop.  As such, we track the seen state of
                // all of the entries and exit the loop if we try to
                // kick out an object that has already been seen.  The
                // first step in this process is thus to clear the seen
                // state for all of the entries.
                foreach (var entry in this.hashTable.Values)
                {
                    entry.Seen = false;
                }

                // Try to put the entry into its Hash2 location.  If
                // that fails then kick the entry in Hash2 to that
                // entry's Hash2 location.  Repeat until we either find
                // an empty Hash2 slot or try to kick an entry that is
                // already in its Hash2 location into its Hash2 location
                // (which would cause an infinite loop).
                if (!this.TryAssignAndKickOutIfNeeded(hashEntry))
                {
                    return false;
                }
            }

            // We were successful in adding this entry to the hash table.
            return true;
        }

        /// <summary>
        /// Tries to get the value associated with the specified key.
        /// </summary>
        /// <param name="key">The key whose value to get.</param>
        /// <param name="value">The value associated with the specified
        /// key.</param>
        /// <returns>true if the key was found in the hash table; false
        /// otherwise.</returns>
        public bool TryGetValue(int key, out TValue value)
        {
            CuckooHashEntry hashEntry;
            bool found = this.hashTable.TryGetValue(key, out hashEntry);
            value = found ? hashEntry.Value : default(TValue);
            return found;
        }

        /// <summary>
        /// Clear the hash table.
        /// </summary>
        public void Clear()
        {
            this.hashTable.Clear();
            this.NumValuesStoredAtFirstHash = 0;
            this.NumValuesStoredAtSecondHash = 0;
        }

        /// <summary>
        /// Try to store the entry into its Hash2 location.  If that
        /// fails then kick the entry in Hash2 to that entry's Hash2
        /// location.  Recurse until we either find an empty Hash2 slot
        /// or try to kick an entry that is already in its Hash2
        /// location into its Hash2 location (which would cause an
        /// infinite loop).
        /// </summary>
        /// <param name="hashEntry">The hash entry to store.</param>
        /// <returns>true if the hash entry was stored at its Hash2
        /// location; false if storing the hash entry at its Hash2
        /// location would cause an infinite loop.</returns>
        private bool TryAssignAndKickOutIfNeeded(CuckooHashEntry hashEntry)
        {
            // Success is immediate if the Hash2 location is unused.
            if (!this.hashTable.ContainsKey(hashEntry.Hash2))
            {
                this.NumValuesStoredAtSecondHash++;
                this.hashTable[hashEntry.Hash2] = hashEntry;
                return true;
            }

            // Get the kicked out entry and fail if we have already seen
            // that entry.
            var kickedOutEntry = this.hashTable[hashEntry.Hash2];
            if (kickedOutEntry.Seen)
            {
                return false;
            }

            // Put the new word in the kicked out word's spot.
            this.hashTable[hashEntry.Hash2] = hashEntry;
            hashEntry.Seen = true;
            this.NumValuesStoredAtFirstHash--;
            this.NumValuesStoredAtSecondHash++;

            // Try to store the kicked out entry in its Hash2 location,
            // kicking out additional words as necessary.
            kickedOutEntry.Seen = true;
            return this.TryAssignAndKickOutIfNeeded(kickedOutEntry);
        }

        /// <summary>
        /// Stores a single entry in a Cuckoo Hash table.
        /// </summary>
        private class CuckooHashEntry
        {
            /// <summary>
            /// Initializes a new instance of the CuckooHashEntry class.
            /// </summary>
            /// <param name="hash1">First hash value for this
            /// entry.</param>
            /// <param name="hash2">Second hash value for this
            /// entry.</param>
            /// <param name="value">Value of this entry.</param>
            public CuckooHashEntry(int hash1, int hash2, TValue value)
            {
                this.Hash1 = hash1;
                this.Hash2 = hash2;
                this.Value = value;
            }

            /// <summary>
            /// Gets the first hash value of this entry.
            /// </summary>
            public int Hash1 { get; private set; }

            /// <summary>
            /// Gets the second hash value of this entry.
            /// </summary>
            public int Hash2 { get; private set; }

            /// <summary>
            /// Gets the value of this entry.
            /// </summary>
            public TValue Value { get; private set; }

            /// <summary>
            /// Gets or sets a value indicating whether this entry has
            /// been seen by the "kick out" routine.
            /// </summary>
            public bool Seen { get; set; }
        }
    }
}
