using System;
using System.Collections.Generic;

namespace MultiAgentLanguageModels.Other
{
    public class Node
    {
        public static Node ForbiddenNode { get {
                if (_forbiddenNode == null)
                    _forbiddenNode = new Node();
                return _forbiddenNode;
            }
        }
        public IEnumerable<Tuple<string, bool>> FluentsValues { get; }

        private readonly bool forbiddenNode = false;

        private static Node _forbiddenNode;

        public Node(IEnumerable<Tuple<string, bool>> fluentsValues)
        {
            FluentsValues = fluentsValues;
        }

        private Node()
        {
            forbiddenNode = true;
        }
    }
}