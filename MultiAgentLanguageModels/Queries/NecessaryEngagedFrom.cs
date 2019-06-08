﻿using System.Collections.Generic;
using System.Linq;
using MultiAgentLanguageModels.Expressions;
using MultiAgentLanguageModels.Reasoning;

namespace MultiAgentLanguageModels.Queries
{
    public class NecessaryEngagedFrom : Query
    {
        public AgentsList Agents { get; }
        public Instruction Instructions { get; }
        public LogicExpression Condition { get; }

        public NecessaryEngagedFrom(AgentsList agents, Instruction instructions, LogicExpression condition)
        {
            Agents = agents;
            Instructions = instructions;
            Condition = condition;
        }

        public override bool Solve(ExpressionsList expressions)
        {
            var reasoningEngine = new ReasoningEngine();
            var res = reasoningEngine.Res(expressions);
            var initialStates = reasoningEngine.InitialStates(expressions);
            var allStates = reasoningEngine.PossibleStates(expressions);
            var piCondition = Condition.EvaluateLogicExpression();

            // TODO: implement

            return true;
        }
    }

    public class NecessaryEngaged : NecessaryEngagedFrom
    {
        public NecessaryEngaged(AgentsList agents, Instruction instructions)
            : base(agents, instructions, new True())
        {
        }
    }
}