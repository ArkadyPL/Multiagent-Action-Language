namespace MultiAgentLanguageModels.Other
{
    public class Edge
    {
        public Action Action { get; }
        public Node From { get; }
        public Node To { get; }
        public AgentsList Agents { get; }
        public Edge(Node from, Node to, Action action, AgentsList agents)
        {
            Action = action;
            From = from;
            To = to;
            Agents = agents;
        }
    }
}