#include <string>
#include <iostream>
#include <vector>
#include <map>
#include <memory>
#include <cctype>
#include <cstdio>
#include <utility>
#include <cstdlib>

using namespace std;

// Scanner
enum Token {
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5   
};

// not the best choice
static string IdentifierStr;
static double NumVal;

// Scanner implementation
static int gettok() {
    static int LastChar = ' ';

    // skip spaces
    while (isspace(LastChar)) {
        LastChar = getchar();
    }
    // Get Identifier
    // standard identifiers [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum(LastChar = getchar())) {
            IdentifierStr += LastChar;
        }
        if (IdentifierStr == "def"){
            return tok_def;
        }
        if (IdentifierStr == "extern"){
            return tok_extern;
        }
        return tok_identifier;
    }
    // Get Numbers, more is needed will incorrectly handle 1.23.45 ....
    if (isdigit(LastChar) || LastChar == '.') {
        string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');
        NumVal = strtod(NumStr.c_str(), 0); // convert to numeric using strtod
        return tok_number;
    }
    // Processing Comments, Skipts to end of the line when # is read
    if (LastChar == '#') {
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        
        if (LastChar != EOF) {
            return gettok();
        }
    }
    // End of File check
    if (LastChar == EOF) {
        return tok_eof;
    }
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

// Base class for all expression nodes, better lang would use a type field
class ExprAST {
    public:
        virtual ~ExprAST() = default;
};

// Expressuon class for numberic literals like "1.0"
class NumberExprAST : public ExprAST {
    double Val;
    public:
        NumberExprAST(double Val) : Val(Val) {}
};

//Expression Class for referencing a variable like "a"
class VariableExprAST : public ExprAST {
    string Name;
    public:
        VariableExprAST(const string &Name) : Name(Name) {}
};

// Expression class for a binary operator
class BinaryExprAST : public ExprAST {
    char Op;
    unique_ptr<ExprAST> LHS, RHS;
    public:
        BinaryExprAST(
            char op, unique_ptr<ExprAST> LHS, 
            unique_ptr<ExprAST> RHS
        ) : Op(op), LHS(move(LHS)), RHS(move(RHS)) {}
};

// Expression Class for function calls
class CallExprAST : public ExprAST {
    string Callee;
    vector<unique_ptr<ExprAST>> Args;
    public:
        CallExprAST(
            const string &Callee,
            vector<unique_ptr<ExprAST>> Args
        ) : Callee(Callee), Args(move(Args)) {}
};

/*
    Class represents the "prototype" for a function,
    which Captures Its name, and its argument names (thus implicitly
    the number of arguments the function takes)
*/

class PrototypeAST {
    string Name;
    vector<string> Args;
    public:
        PrototypeAST(
            const string &Name,
            vector<string> Args
        ) : Name(Name), Args(move(Args)) {}
        const string &getName() const { 
            return Name; 
        }
};

// This class represents a function definition
class FunctionAST {
    unique_ptr<PrototypeAST> Proto;
    unique_ptr<ExprAST> Body;
    public:
        FunctionAST(
            unique_ptr<PrototypeAST> Proto,
            unique_ptr<ExprAST> Body
        ) : Proto(move(Proto)), Body(move(Body)) {}
};

// Token Buffer
// Current token the parser is looking at
static int CurTok;
static int getNextToken() {
    return CurTok = gettok();
}

// These are little helper functions for error handling
// Log error routines are simple helper routines
unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "Erro: %s\n", Str);
    return nullptr;
}

unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogError(Str);
    return nullptr;
}
// Recursive decent Parser: Expression Parsing

// Number expr := number
static unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consume num
    return move(Result);
}

// Parenthese expr = '(' expression ')'
static unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // eat (
    auto V = ParseExpression(); // Where is this defined?
    if (!V) {
        return nullptr;
    }
    if (CurTok != ')') {
        return LogError("expected ')'");
    }
    getNextToken();
    return V;
}

// Identifier Expr 
// = identifier
// = identifier '(' expression ')'
static unique_ptr<ExprAST> ParseIdentifierExpr() {
    string IdName = IdentifierStr;
    getNextToken();
    if (CurTok != '(') {
        return make_unique<VariableExprAST>(IdName);
    }
    getNextToken();
    vector<unique_ptr<ExprAST>> Args;
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression()) {
                Args.push_back(move(Arg));
            } else {
                return nullptr;
            }
            if (CurTok == ')') {
                break;
            }
            if (CurTok != ',') {
                return LogError("Expected ')' or ',' in argument list");
            }
            getNextToken();
        }
    }
    // Eat ')'
    getNextToken();
    return make_unique<CallExprAST>(IdName, move(Args));
}

// Primary
// = identifier expr
// = number expr
// = parenthese expr
static unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
        default:
            return LogError("Unkown token when expecting an expression");
    }
}

// Binary Expression Parsing

//Binop Precedence - This holds the precedence for each binary operator 
// that is defined
static map<char, int> BinopPrecedence;

// Get the Precedence of the pending binary operator token
static int GetTokPrecedence() {
    if (!isascii(CurTok)) {
        return -1;
    }
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) {
        return -1;
    }
    return TokPrec;
} 

// Expression = Primary Binoprhs
static unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS) {
        return nullptr;
    }
    return ParseBinOpRHS(0, move(LHS));
}

// BinOpRHS
// = ('+', primary)
static unique_ptr<ExprAST> ParseBinOpRHS(
    int ExprPrec, unique_ptr<ExprAST> LHS
    ) {
    // if binop find precedence
    while (true) {
        int TokPrec = GetTokPrecedence();
        // If this is a binop that binds at least as tightly as the curr
        // binop, consume it, otherwise we done
        if (TokPrec < ExprPrec) {
            return LHS;
        }
        int BinOp = CurTok; // we know this is binOp
        getNextToken(); // eat BinOp        
        // parse primary expression after the binary op
        auto RHS = ParsePrimary();
        if (!RHS) {
            return nullptr;
        }
        // If BinOp binds less tightly with RHS than the op after RHS,
        // let the pending op take RHS as its LHS
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }

        // Merge LHS/RHS
        LHS = make_unique<BinaryExprAST>(BinOp, move(LHS), move(RHS));
    }
}

// Prototype
// = id '(' id ')'
static unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier) {
        return LogErrorP("Expected function name in prototype");
    }
    string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(') {
        return LogErrorP("Expected '(' in prototype");
    }
    
    // read the list of arg names
    vector<string> ArgNames;
    while (getNextToken() == tok_identifier) {
        ArgNames.push_back(IdentifierStr);
    }
    if (CurTok != ')') {
        return LogErrorP("Expected ')' in prototype");
    }
    // success
    getNextToken();
    return make_unique<PrototypeAST>(FnName, move(ArgNames));
}

// Definition
// = 'def' prototype expression
static unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken(); //eat def
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }
    if (auto E = ParseExpression()) {
        return make_unique<FunctionAST>(move(Proto), move(E));
    }
    return nullptr;
}

// External
// = 'extern' prototype
static unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken(); // eat extern
    return ParsePrototype();
}

// Top level lex
// = expression
static unique_ptr<FunctionAST> ParseTopLevelExpr () {
    if (auto E = ParseExpression()) {
        // make an anon proto
        auto Proto = make_unique<PrototypeAST>("", vector<string>());
        return make_unique<FunctionAST>(move(Proto), move(E));
    }
}

// top 
// = definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

static void HandleDefinition() {
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

int main() {
    // 1 is lowest precedence ---- Extend this ****
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest

    // Prime the first token
    fprintf(stderr, "Ready --> ");
    getNextToken();

    MainLoop();

    return 0;
}

