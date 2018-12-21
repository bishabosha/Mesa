// Generated from EEC.g4 by ANTLR 4.7.1

import java.util.*;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class EECParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, FIXITY=5, ASSIGN=6, SUB=7, OPERATOR=8, 
		Id=9, IntegerLiteral=10, FloatingPointLiteral=11, Sep=12, Semi=13, NL=14, 
		NEWLINE=15, WS=16, COMMENT=17, LINE_COMMENT=18;
	public static final int
		RULE_literal = 0, RULE_primary = 1, RULE_qualId = 2, RULE_expr = 3, RULE_operator = 4, 
		RULE_prefixExpr = 5, RULE_fixity = 6, RULE_moduleInfo = 7, RULE_topStatSeq = 8, 
		RULE_statSeq = 9, RULE_topStat = 10, RULE_stat = 11, RULE_translationUnit = 12;
	public static final String[] ruleNames = {
		"literal", "primary", "qualId", "expr", "operator", "prefixExpr", "fixity", 
		"moduleInfo", "topStatSeq", "statSeq", "topStat", "stat", "translationUnit"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "'.'", "'module'", null, "'='", "'-'", null, null, 
		null, null, null, "';'", "'\n'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, "FIXITY", "ASSIGN", "SUB", "OPERATOR", "Id", 
		"IntegerLiteral", "FloatingPointLiteral", "Sep", "Semi", "NL", "NEWLINE", 
		"WS", "COMMENT", "LINE_COMMENT"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "EEC.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }


	Map<String, String> fmap = new HashMap<>();
	Map<String, Integer> pmap = new HashMap<>();

	public int nextp (String op) {
		String fix = fmap.get(op);
		Integer p = pmap.get(op);
		if (fix.equals("infixr")) return p;
		if (fix.equals("infixl")) return p+1;
		if (fix.equals("prefix")) return p;
		if (fix.equals("postfix")) return p+1;
		if (fix.equals("infix")) return p+1;
		return 0;
	}

	public void updateFix(String f, int p, Iterable<Token> op) {
		for (Token t: op) {
			fmap.put(t.getText(), f);
			pmap.put(t.getText(), p);
			System.out.println (String.format("Putting %s as %s %d", t.getText(), f, p));
		}
	}

	public boolean prefix(String text) {
		return fmap.get(text).equals("prefix");
	}

	public boolean postfix(String text, int p) {
		return fmap.get(text).equals("postfix")
			&& pmap.get(text) >= p;
	}

	public boolean infix(String text, int p) {
		return fmap.get(text).contains("infix")
			&& pmap.get(text) >= p;
	}

	public boolean notInfixNoAssoc(String op) {
		return !fmap.get(op).equals("infix");
	}

	public EECParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class LiteralContext extends ParserRuleContext {
		public TerminalNode IntegerLiteral() { return getToken(EECParser.IntegerLiteral, 0); }
		public TerminalNode SUB() { return getToken(EECParser.SUB, 0); }
		public TerminalNode FloatingPointLiteral() { return getToken(EECParser.FloatingPointLiteral, 0); }
		public LiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_literal; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitLiteral(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LiteralContext literal() throws RecognitionException {
		LiteralContext _localctx = new LiteralContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_literal);
		int _la;
		try {
			setState(34);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(27);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==SUB) {
					{
					setState(26);
					match(SUB);
					}
				}

				setState(29);
				match(IntegerLiteral);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(31);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==SUB) {
					{
					setState(30);
					match(SUB);
					}
				}

				setState(33);
				match(FloatingPointLiteral);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PrimaryContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public PrimaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_primary; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitPrimary(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PrimaryContext primary() throws RecognitionException {
		PrimaryContext _localctx = new PrimaryContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_primary);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(36);
			match(T__0);
			setState(37);
			expr(0);
			setState(38);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QualIdContext extends ParserRuleContext {
		public List<TerminalNode> Id() { return getTokens(EECParser.Id); }
		public TerminalNode Id(int i) {
			return getToken(EECParser.Id, i);
		}
		public QualIdContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualId; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitQualId(this);
			else return visitor.visitChildren(this);
		}
	}

	public final QualIdContext qualId() throws RecognitionException {
		QualIdContext _localctx = new QualIdContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_qualId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(40);
			match(Id);
			setState(45);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2) {
				{
				{
				setState(41);
				match(T__2);
				setState(42);
				match(Id);
				}
				}
				setState(47);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprContext extends ParserRuleContext {
		public int p;
		public OperatorContext op;
		public LiteralContext literal() {
			return getRuleContext(LiteralContext.class,0);
		}
		public PrimaryContext primary() {
			return getRuleContext(PrimaryContext.class,0);
		}
		public PrefixExprContext prefixExpr() {
			return getRuleContext(PrefixExprContext.class,0);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<OperatorContext> operator() {
			return getRuleContexts(OperatorContext.class);
		}
		public OperatorContext operator(int i) {
			return getRuleContext(OperatorContext.class,i);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public ExprContext(ParserRuleContext parent, int invokingState, int p) {
			super(parent, invokingState);
			this.p = p;
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr(int p) throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState(), p);
		enterRule(_localctx, 6, RULE_expr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(51);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				{
				setState(48);
				literal();
				}
				break;
			case 2:
				{
				setState(49);
				primary();
				}
				break;
			case 3:
				{
				setState(50);
				prefixExpr();
				}
				break;
			}
			setState(62);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					setState(60);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
					case 1:
						{
						setState(53);
						if (!(infix(_input.LT(1).getText(), _localctx.p))) throw new FailedPredicateException(this, "infix(_input.LT(1).getText(), $p)");
						setState(54);
						((ExprContext)_localctx).op = operator();
						setState(55);
						expr(nextp((((ExprContext)_localctx).op!=null?_input.getText(((ExprContext)_localctx).op.start,((ExprContext)_localctx).op.stop):null)));
						setState(56);
						if (!(notInfixNoAssoc((((ExprContext)_localctx).op!=null?_input.getText(((ExprContext)_localctx).op.start,((ExprContext)_localctx).op.stop):null)))) throw new FailedPredicateException(this, "notInfixNoAssoc($op.text)");
						}
						break;
					case 2:
						{
						setState(58);
						if (!(postfix(_input.LT(1).getText(), _localctx.p))) throw new FailedPredicateException(this, "postfix(_input.LT(1).getText(), $p)");
						setState(59);
						operator();
						}
						break;
					}
					} 
				}
				setState(64);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OperatorContext extends ParserRuleContext {
		public TerminalNode SUB() { return getToken(EECParser.SUB, 0); }
		public TerminalNode OPERATOR() { return getToken(EECParser.OPERATOR, 0); }
		public OperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_operator; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitOperator(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OperatorContext operator() throws RecognitionException {
		OperatorContext _localctx = new OperatorContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_operator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(65);
			_la = _input.LA(1);
			if ( !(_la==SUB || _la==OPERATOR) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PrefixExprContext extends ParserRuleContext {
		public Token op;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode OPERATOR() { return getToken(EECParser.OPERATOR, 0); }
		public PrefixExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_prefixExpr; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitPrefixExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PrefixExprContext prefixExpr() throws RecognitionException {
		PrefixExprContext _localctx = new PrefixExprContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_prefixExpr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(67);
			if (!(prefix(_input.LT(1).getText()))) throw new FailedPredicateException(this, "prefix(_input.LT(1).getText())");
			setState(68);
			((PrefixExprContext)_localctx).op = match(OPERATOR);
			setState(69);
			expr(nextp((((PrefixExprContext)_localctx).op!=null?((PrefixExprContext)_localctx).op.getText():null)));
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FixityContext extends ParserRuleContext {
		public Token f;
		public Token p;
		public Token SUB;
		public List<Token> op = new ArrayList<Token>();
		public Token OPERATOR;
		public Token _tset149;
		public TerminalNode FIXITY() { return getToken(EECParser.FIXITY, 0); }
		public TerminalNode IntegerLiteral() { return getToken(EECParser.IntegerLiteral, 0); }
		public List<TerminalNode> SUB() { return getTokens(EECParser.SUB); }
		public TerminalNode SUB(int i) {
			return getToken(EECParser.SUB, i);
		}
		public List<TerminalNode> OPERATOR() { return getTokens(EECParser.OPERATOR); }
		public TerminalNode OPERATOR(int i) {
			return getToken(EECParser.OPERATOR, i);
		}
		public FixityContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_fixity; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitFixity(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FixityContext fixity() throws RecognitionException {
		FixityContext _localctx = new FixityContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_fixity);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(71);
			((FixityContext)_localctx).f = match(FIXITY);
			setState(72);
			((FixityContext)_localctx).p = match(IntegerLiteral);
			setState(74); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(73);
				((FixityContext)_localctx)._tset149 = _input.LT(1);
				_la = _input.LA(1);
				if ( !(_la==SUB || _la==OPERATOR) ) {
					((FixityContext)_localctx)._tset149 = (Token)_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				((FixityContext)_localctx).op.add(((FixityContext)_localctx)._tset149);
				}
				}
				setState(76); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==SUB || _la==OPERATOR );
			updateFix((((FixityContext)_localctx).f!=null?((FixityContext)_localctx).f.getText():null), (((FixityContext)_localctx).p!=null?Integer.valueOf(((FixityContext)_localctx).p.getText()):0), ((FixityContext)_localctx).op);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ModuleInfoContext extends ParserRuleContext {
		public QualIdContext qualId() {
			return getRuleContext(QualIdContext.class,0);
		}
		public ModuleInfoContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_moduleInfo; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitModuleInfo(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ModuleInfoContext moduleInfo() throws RecognitionException {
		ModuleInfoContext _localctx = new ModuleInfoContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_moduleInfo);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(80);
			match(T__3);
			setState(81);
			qualId();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TopStatSeqContext extends ParserRuleContext {
		public List<TopStatContext> topStat() {
			return getRuleContexts(TopStatContext.class);
		}
		public TopStatContext topStat(int i) {
			return getRuleContext(TopStatContext.class,i);
		}
		public List<TerminalNode> Sep() { return getTokens(EECParser.Sep); }
		public TerminalNode Sep(int i) {
			return getToken(EECParser.Sep, i);
		}
		public TopStatSeqContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_topStatSeq; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitTopStatSeq(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TopStatSeqContext topStatSeq() throws RecognitionException {
		TopStatSeqContext _localctx = new TopStatSeqContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_topStatSeq);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(83);
			topStat();
			setState(88);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(84);
					match(Sep);
					setState(85);
					topStat();
					}
					} 
				}
				setState(90);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StatSeqContext extends ParserRuleContext {
		public List<StatContext> stat() {
			return getRuleContexts(StatContext.class);
		}
		public StatContext stat(int i) {
			return getRuleContext(StatContext.class,i);
		}
		public List<TerminalNode> Sep() { return getTokens(EECParser.Sep); }
		public TerminalNode Sep(int i) {
			return getToken(EECParser.Sep, i);
		}
		public StatSeqContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_statSeq; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitStatSeq(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StatSeqContext statSeq() throws RecognitionException {
		StatSeqContext _localctx = new StatSeqContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_statSeq);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(91);
			stat();
			setState(96);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(92);
					match(Sep);
					setState(93);
					stat();
					}
					} 
				}
				setState(98);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TopStatContext extends ParserRuleContext {
		public FixityContext fixity() {
			return getRuleContext(FixityContext.class,0);
		}
		public TopStatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_topStat; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitTopStat(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TopStatContext topStat() throws RecognitionException {
		TopStatContext _localctx = new TopStatContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_topStat);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(99);
			fixity();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StatContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public StatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stat; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitStat(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StatContext stat() throws RecognitionException {
		StatContext _localctx = new StatContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_stat);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(101);
			expr(0);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TranslationUnitContext extends ParserRuleContext {
		public TopStatSeqContext topStatSeq() {
			return getRuleContext(TopStatSeqContext.class,0);
		}
		public List<TerminalNode> Sep() { return getTokens(EECParser.Sep); }
		public TerminalNode Sep(int i) {
			return getToken(EECParser.Sep, i);
		}
		public StatSeqContext statSeq() {
			return getRuleContext(StatSeqContext.class,0);
		}
		public List<ModuleInfoContext> moduleInfo() {
			return getRuleContexts(ModuleInfoContext.class);
		}
		public ModuleInfoContext moduleInfo(int i) {
			return getRuleContext(ModuleInfoContext.class,i);
		}
		public TranslationUnitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_translationUnit; }
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof EECVisitor ) return ((EECVisitor<? extends T>)visitor).visitTranslationUnit(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TranslationUnitContext translationUnit() throws RecognitionException {
		TranslationUnitContext _localctx = new TranslationUnitContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_translationUnit);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(109);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__3) {
				{
				{
				setState(103);
				moduleInfo();
				setState(105);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Sep) {
					{
					setState(104);
					match(Sep);
					}
				}

				}
				}
				setState(111);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(112);
			topStatSeq();
			setState(113);
			match(Sep);
			setState(114);
			statSeq();
			setState(116);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Sep) {
				{
				setState(115);
				match(Sep);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 3:
			return expr_sempred((ExprContext)_localctx, predIndex);
		case 5:
			return prefixExpr_sempred((PrefixExprContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean expr_sempred(ExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return infix(_input.LT(1).getText(), _localctx.p);
		case 1:
			return notInfixNoAssoc((((ExprContext)_localctx).op!=null?_input.getText(((ExprContext)_localctx).op.start,((ExprContext)_localctx).op.stop):null));
		case 2:
			return postfix(_input.LT(1).getText(), _localctx.p);
		}
		return true;
	}
	private boolean prefixExpr_sempred(PrefixExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 3:
			return prefix(_input.LT(1).getText());
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\24y\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4"+
		"\f\t\f\4\r\t\r\4\16\t\16\3\2\5\2\36\n\2\3\2\3\2\5\2\"\n\2\3\2\5\2%\n\2"+
		"\3\3\3\3\3\3\3\3\3\4\3\4\3\4\7\4.\n\4\f\4\16\4\61\13\4\3\5\3\5\3\5\5\5"+
		"\66\n\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\7\5?\n\5\f\5\16\5B\13\5\3\6\3\6\3"+
		"\7\3\7\3\7\3\7\3\b\3\b\3\b\6\bM\n\b\r\b\16\bN\3\b\3\b\3\t\3\t\3\t\3\n"+
		"\3\n\3\n\7\nY\n\n\f\n\16\n\\\13\n\3\13\3\13\3\13\7\13a\n\13\f\13\16\13"+
		"d\13\13\3\f\3\f\3\r\3\r\3\16\3\16\5\16l\n\16\7\16n\n\16\f\16\16\16q\13"+
		"\16\3\16\3\16\3\16\3\16\5\16w\n\16\3\16\2\2\17\2\4\6\b\n\f\16\20\22\24"+
		"\26\30\32\2\3\3\2\t\n\2y\2$\3\2\2\2\4&\3\2\2\2\6*\3\2\2\2\b\65\3\2\2\2"+
		"\nC\3\2\2\2\fE\3\2\2\2\16I\3\2\2\2\20R\3\2\2\2\22U\3\2\2\2\24]\3\2\2\2"+
		"\26e\3\2\2\2\30g\3\2\2\2\32o\3\2\2\2\34\36\7\t\2\2\35\34\3\2\2\2\35\36"+
		"\3\2\2\2\36\37\3\2\2\2\37%\7\f\2\2 \"\7\t\2\2! \3\2\2\2!\"\3\2\2\2\"#"+
		"\3\2\2\2#%\7\r\2\2$\35\3\2\2\2$!\3\2\2\2%\3\3\2\2\2&\'\7\3\2\2\'(\5\b"+
		"\5\2()\7\4\2\2)\5\3\2\2\2*/\7\13\2\2+,\7\5\2\2,.\7\13\2\2-+\3\2\2\2.\61"+
		"\3\2\2\2/-\3\2\2\2/\60\3\2\2\2\60\7\3\2\2\2\61/\3\2\2\2\62\66\5\2\2\2"+
		"\63\66\5\4\3\2\64\66\5\f\7\2\65\62\3\2\2\2\65\63\3\2\2\2\65\64\3\2\2\2"+
		"\66@\3\2\2\2\678\6\5\2\389\5\n\6\29:\5\b\5\2:;\6\5\3\3;?\3\2\2\2<=\6\5"+
		"\4\3=?\5\n\6\2>\67\3\2\2\2><\3\2\2\2?B\3\2\2\2@>\3\2\2\2@A\3\2\2\2A\t"+
		"\3\2\2\2B@\3\2\2\2CD\t\2\2\2D\13\3\2\2\2EF\6\7\5\2FG\7\n\2\2GH\5\b\5\2"+
		"H\r\3\2\2\2IJ\7\7\2\2JL\7\f\2\2KM\t\2\2\2LK\3\2\2\2MN\3\2\2\2NL\3\2\2"+
		"\2NO\3\2\2\2OP\3\2\2\2PQ\b\b\1\2Q\17\3\2\2\2RS\7\6\2\2ST\5\6\4\2T\21\3"+
		"\2\2\2UZ\5\26\f\2VW\7\16\2\2WY\5\26\f\2XV\3\2\2\2Y\\\3\2\2\2ZX\3\2\2\2"+
		"Z[\3\2\2\2[\23\3\2\2\2\\Z\3\2\2\2]b\5\30\r\2^_\7\16\2\2_a\5\30\r\2`^\3"+
		"\2\2\2ad\3\2\2\2b`\3\2\2\2bc\3\2\2\2c\25\3\2\2\2db\3\2\2\2ef\5\16\b\2"+
		"f\27\3\2\2\2gh\5\b\5\2h\31\3\2\2\2ik\5\20\t\2jl\7\16\2\2kj\3\2\2\2kl\3"+
		"\2\2\2ln\3\2\2\2mi\3\2\2\2nq\3\2\2\2om\3\2\2\2op\3\2\2\2pr\3\2\2\2qo\3"+
		"\2\2\2rs\5\22\n\2st\7\16\2\2tv\5\24\13\2uw\7\16\2\2vu\3\2\2\2vw\3\2\2"+
		"\2w\33\3\2\2\2\17\35!$/\65>@NZbkov";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}