﻿using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace MultiAgentLanguageModels.Queries
{
    public abstract class Query
    {
        public abstract bool Interpret(IEnumerable<bool> allPossibilities);
    }
}
