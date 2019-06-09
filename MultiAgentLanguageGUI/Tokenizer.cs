using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MultiAgentLanguageGUI
{
    public enum TokenType
    {
        Prototype,
        Name, Operator,
        Action, Agent, Fluent,
        Keyword
    }

    public class TokenException : Exception
    {
        public int LineNumber { get; private set; }
        public int ColumnNumber { get; private set; }
        public int TokenLength { get; private set; }

        public TokenException(string message, int lineNumber, int columnNumber, int tokenLength) : base(message)
        {
            LineNumber = lineNumber;
            ColumnNumber = columnNumber;
            TokenLength = tokenLength;
        }
    }

    public class Token : ICloneable
    {
        public TokenType Type { get; set; }
        public string Name { get; set; }
        public int LineNumber { get; set; }
        public int ColumnNumber { get; set; }

        public Token(TokenType type, string name, int lineNumber, int columnNumber)
        {
            Type = type;
            Name = name;
            LineNumber = lineNumber;
            ColumnNumber = columnNumber;
        }

        public Token(int lineNumber, int columnNumber) : this(TokenType.Prototype, "", lineNumber, columnNumber)
        {

        }

        public void ThrowException(string content)
        {
            throw new TokenException(
                $"Exception thrown at token created at line {LineNumber}, column {ColumnNumber}:\n{content}\nToken content: {Name}",
                LineNumber, ColumnNumber, Name.Length);
        }

        public object Clone()
        {
            return new Token(Type, Name, LineNumber, ColumnNumber);
        }
    }

    public class Tokenizer
    {
        // wpisywanie poniższych słów kluczowych skutkuje stworzeniem tokenów o typie innym niż TokenType.Name
        // przy nazwach złożonych z liter, cyfr i podkreślnika
        public static readonly Dictionary<string, TokenType> Keyword = new Dictionary<string, TokenType>
        {
            { "Action", TokenType.Action },
            { "Agent", TokenType.Agent },
            { "Fluent", TokenType.Fluent },
            { "initially", TokenType.Keyword },
            { "noninertial", TokenType.Keyword },
            { "by", TokenType.Keyword },
            { "causes", TokenType.Keyword },
            { "releases", TokenType.Keyword },
            { "if", TokenType.Keyword },
            { "impossible", TokenType.Keyword },
            { "always", TokenType.Keyword },
            { "not", TokenType.Keyword },
            {"after", TokenType.Keyword },
            {"observable", TokenType.Keyword }
        };
        // poniższe znaki traktowane są jako znaki operatorów i powodują przerwanie tworzenia tokena z alfanumerycznymi znakami
        // wszystkie znaki niebędące na poniższej liście są traktowane jako części nazw/keywordów w przypadku liter, cyfr i _
        // lub whitespace'y dla wszystkiego innego
        public static readonly char[] OperatorCharacter = { '+', '-', '>', '<', '|', '&', '^', '(', ')', '[', ']', ',', '~', '.' };
        // przypisanie typów tokenów do tekstowych operatorów, najdłuższe powinny być na początku
        // każdy operator nie na liście jest traktowany jako błąd w czasie tokenizacji
        public static readonly Tuple<string, TokenType>[] LegalOperator =
        {
            Tuple.Create( "<->", TokenType.Operator ),
            Tuple.Create( "->", TokenType.Operator ),
            Tuple.Create( "&&", TokenType.Operator ),
            Tuple.Create( "||", TokenType.Operator ),
            Tuple.Create( "~", TokenType.Operator ),
            Tuple.Create( ".", TokenType.Operator ),
            Tuple.Create( ",", TokenType.Operator ),
            Tuple.Create( "(", TokenType.Operator ),
            Tuple.Create( ")", TokenType.Operator ),
            Tuple.Create( "[", TokenType.Operator ),
            Tuple.Create( "]", TokenType.Operator )
        };

        public enum CharacterCategory
        {
            Whitespace,
            Alphanumeric,
            Operator
        }

        static int LineNumber;
        static int ColumnNumber;

        static CharacterCategory GetCharacterType(char input)
        {
            if(Char.IsLetterOrDigit(input) || input == '_')
            {
                return CharacterCategory.Alphanumeric;
            }
            if(OperatorCharacter.Contains(input))
            {
                return CharacterCategory.Operator;
            }
            return CharacterCategory.Whitespace;
        }

        static void DumpToken(List<Token> target, Token addition)
        {
            if(addition != null)
            {
                if(addition.Name == "")
                {
                    addition.ThrowException("Empty token");
                }

                if(GetCharacterType(addition.Name[0]) == CharacterCategory.Alphanumeric)
                {
                    if(Keyword.ContainsKey(addition.Name))
                    {
                        addition.Type = Keyword[addition.Name];
                    }
                    else
                    {
                        addition.Type = TokenType.Name;
                    }
                    target.Add(addition);
                }
                else
                {
                    string buffer = addition.Name;
                    while(buffer!="")
                    {
                        int i;
                        for(i = 0; i < LegalOperator.Length; i ++)
                        {
                            if(buffer.StartsWith(LegalOperator[i].Item1))
                            {
                                Token token = (Token)addition.Clone();
                                token.Type = LegalOperator[i].Item2;
                                token.Name = LegalOperator[i].Item1;
                                target.Add(token);
                                buffer = buffer.Remove(0, LegalOperator[i].Item1.Length);
                                break;
                            }
                        }
                        if(i >= LegalOperator.Length)
                        {
                            addition.ThrowException("Illegal operator");
                        }
                    }
                }
            }
        }

        public static List<Token> Tokenize(string input)
        {
            List<Token> list = new List<Token>();
            Token workingToken = null;
            CharacterCategory lastNameType = CharacterCategory.Whitespace;

            bool doubleslash = false;
            bool comment = false;

            LineNumber = 1;
            ColumnNumber = 0;

            for (int i = 0; i < input.Length; i ++)
            {
                char inChar = input[i];
                ColumnNumber++;
                if(inChar == '/')
                {
                    if(doubleslash)
                    {
                        comment = true;
                    }
                    else
                    {
                        doubleslash = true;
                    }
                }
                else
                {
                    doubleslash = false;
                }
                if(comment)
                {
                    if (inChar == '\n')
                    {
                        comment = false;
                    }
                }
                if(!comment)
                switch (GetCharacterType(inChar))
                {
                    case CharacterCategory.Whitespace:
                        DumpToken(list, workingToken);
                        workingToken = null;
                        lastNameType = CharacterCategory.Whitespace;

                        if(inChar == '\n')
                        {
                            doubleslash = false;
                            comment = false;
                            LineNumber++;
                            ColumnNumber = 0;
                        }
                        break;
                    case CharacterCategory.Alphanumeric:
                        if (lastNameType == CharacterCategory.Alphanumeric)
                        {
                            workingToken.Name += inChar;
                        }
                        else
                        {
                            DumpToken(list, workingToken);
                            workingToken = new Token(LineNumber, ColumnNumber);
                            workingToken.Name += inChar;
                            lastNameType = CharacterCategory.Alphanumeric;
                        }
                        break;
                    case CharacterCategory.Operator:
                        if (lastNameType == CharacterCategory.Operator)
                        {
                            workingToken.Name += inChar;
                        }
                        else
                        {
                            DumpToken(list, workingToken);
                            workingToken = new Token(LineNumber, ColumnNumber);
                            workingToken.Name += inChar;
                            lastNameType = CharacterCategory.Operator;
                        }
                        break;
                }
            }
            DumpToken(list, workingToken);
            return list;
        }
    }
}
