import java.util.*;

import static java.lang.Double.NaN;
import static java.lang.Double.valueOf;
import static java.lang.Math.pow;


/*
 *   A calculator for rather simple arithmetic expressions
 *
 *   This is not the program, it's a class declaration (with methods) in it's
 *   own file (which must be named Calculator.java)
 *
 *   NOTE:
 *   - No negative numbers implemented
 */
public class Calculator {

    // Here are the only allowed instance variables!
    // Error messages (more on static later)
    final static String MISSING_OPERAND = "Missing or bad operand";
    final static String DIV_BY_ZERO = "Division with 0";
    final static String MISSING_OPERATOR = "Missing operator or parenthesis";
    final static String OP_NOT_FOUND = "Operator not found";

    // Definition of operators
    final static String OPERATORS = "+-*/^";

    // Method used in REPL
    double eval(String expr) {
        if (expr.length() == 0) {
            return NaN;
        }
        List<String> tokens = tokenize(expr);
        List<String> postfix = infix2Postfix(tokens);
        return evalPostfix(postfix);
    }

    // ------  Evaluate RPN expression -------------------

    double evalPostfix(List<String> postfix) {
        // TODO
        Stack<Double> result = new Stack<>();

        for (String current : postfix){
            if (isOperator(current)){
                double d1 = result.pop();
                double d2 = result.pop();

                result.push(applyOperator(current, d1, d2));
            } else {
                result.push(valueOf(current));
            }
        }

        if (result.size() != 1){

            throw new RuntimeException("fel vid evaluering");
        }
        return result.peek();
    }

    double applyOperator(String op, double d1, double d2) {
        switch (op) {
            case "+":
                return d1 + d2;
            case "-":
                return d2 - d1;
            case "*":
                return d1 * d2;
            case "/":
                if (d1 == 0) {
                    throw new IllegalArgumentException(DIV_BY_ZERO);
                }
                return d2 / d1;
            case "^":
                return pow(d2, d1);
        }
        throw new RuntimeException(OP_NOT_FOUND);
    }

    // ------- Infix 2 Postfix ------------------------
    List<String> infix2Postfix(List<String> infix) {
        Stack<String> operators = new Stack<>();
        List<String> postfix = new ArrayList<>();

        for (String current : infix) {
            if (this.isOperator(current)) {
                while (!operators.empty() && !isRight((String) operators.peek()) && this.hasLowerPrecedence(current, (String) operators.peek())) {
                    postfix.add((String) operators.pop());
                }

                operators.push(current);
            } else if (isRight(current)) {
                operators.push(current);
            } else if (isLeft(current)) {
                doStuff(operators, postfix);
            } else {
                postfix.add(current);
            }
        }

        while(!operators.empty()) {
            postfix.add((String)operators.pop());
        }

        return postfix;
    }

    void doStuff (Stack<String> operators, List<String> postfix) { // annars testa att bara ta bort änd-parantes
        while (!operators.empty()) {
            if (isRight(operators.peek()))  {
                operators.pop();
                return;
            } else {
                postfix.add((String)operators.pop());
            }
        }
    }


    boolean isOperator(String op) {
        return List.of("+", "-", "*", "/", "^").contains(op);
    }

    boolean hasLowerPrecedence(String op1, String op2) {
        return this.getPrecedence(op1) <= this.getPrecedence(op2);
    }

    boolean isRight(String str) {
        return Objects.equals(str, "(");
    }

    boolean isLeft(String str) {
        return Objects.equals(str, ")");
    }


    int getPrecedence(String op) {
        if ("+-".contains(op)) {
            return 2;
        } else if ("*/".contains(op)) {
            return 3;
        } else if ("^".contains(op)) {
            return 4;
        } else {
            throw new RuntimeException(OP_NOT_FOUND);
        }
    }

    Assoc getAssociativity(String op) {
        if ("+-*/".contains(op)) {
            return Assoc.LEFT;
        } else if ("^".contains(op)) {
            return Assoc.RIGHT;
        } else {
            throw new RuntimeException(OP_NOT_FOUND);
        }
    }

    enum Assoc {
        LEFT,
        RIGHT
    }

    // ---------- Tokenize -----------------------

    // List String (not char) because numbers (with many chars)
    List<String> tokenize(String expr) {
        List<String> nn = new ArrayList<>();

        readNumber(nn, expr);
        return removeWhiteSpace(nn); //["34,44,2,5,87"]
    }

    List<String> removeWhiteSpace (List<String> nn) {
        List<String> newList = new ArrayList<>();
        for (String thing : nn) {
            if (!thing.isBlank()) {
                newList.add(thing);
            }
        }
        return newList;
    }

    // Expected is in fact a list [ "1", "+", "10"]
    void readNumber(List<String> numbers, String str){
        StringBuilder sentence = new StringBuilder();
        if (str.length() == 0) { // fugly basfall, luktar illa
            return;
        }

        for (int i = 0; i < str.length(); i++) {
            if (Character.isDigit(str.charAt(i))) {
                sentence.append(str.charAt(i));
            } else {
                numbers.add(sentence.toString());
                sentence = new StringBuilder();
                numbers.add(String.valueOf(str.charAt(i)));
            }
        //numbers.add(sentence.toString());
        //return numbers;
        }
        numbers.add(sentence.toString());
    }
}


