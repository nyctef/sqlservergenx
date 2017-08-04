using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace sqlservergenx
{
    public class DatabaseModel
    {
        private DatabaseModel(ImmutableList<DatabaseModelEntry> databaseModelEntries)
        {
            Entries = databaseModelEntries;
        }

        public static DatabaseModel Empty { get; } = new DatabaseModel(ImmutableList.Create<DatabaseModelEntry>());

        ImmutableList<DatabaseModelEntry> Entries { get; }

        public DatabaseModel WithEntry(DatabaseModelEntry entry)
        {
            return new DatabaseModel(Entries.Add(entry));
        }
    }

    public class DatabaseModelEntry
    {
        public DatabaseModelEntry(string type)
        {
            Type = type;
        }

        public string Type { get; }

        public object foo { get; }
    }
}
