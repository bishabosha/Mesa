// Generated from EEC.g4 by ANTLR 4.7.1

import java.util.*;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link EECParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface EECVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link EECParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteral(EECParser.LiteralContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimary(EECParser.PrimaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#qualId}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQualId(EECParser.QualIdContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(EECParser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#operator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOperator(EECParser.OperatorContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#prefixExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrefixExpr(EECParser.PrefixExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#fixity}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFixity(EECParser.FixityContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#moduleInfo}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitModuleInfo(EECParser.ModuleInfoContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#topStatSeq}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTopStatSeq(EECParser.TopStatSeqContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#statSeq}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatSeq(EECParser.StatSeqContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#topStat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTopStat(EECParser.TopStatContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStat(EECParser.StatContext ctx);
	/**
	 * Visit a parse tree produced by {@link EECParser#translationUnit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTranslationUnit(EECParser.TranslationUnitContext ctx);
}