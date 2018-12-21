// Generated from EEC.g4 by ANTLR 4.7.1

import java.util.*;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class EECLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, FIXITY=5, ASSIGN=6, SUB=7, OPERATOR=8, 
		Id=9, IntegerLiteral=10, FloatingPointLiteral=11, Sep=12, Semi=13, NL=14, 
		NEWLINE=15, WS=16, COMMENT=17, LINE_COMMENT=18;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "T__2", "T__3", "FIXITY", "ASSIGN", "SUB", "OPERATOR", 
		"Id", "IntegerLiteral", "FloatingPointLiteral", "WhiteSpace", "Op", "Opchar", 
		"Idrest", "LongType", "FloatType", "Upper", "Lower", "Letter", "ExponentPart", 
		"DecimalNumeral", "Digit", "NonZeroDigit", "Plainid", "UnicodeClass_LU", 
		"UnicodeClass_LL", "UnicodeClass_LT", "UnicodeClass_LO", "Sep", "Semi", 
		"NL", "NEWLINE", "WS", "COMMENT", "LINE_COMMENT"
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


	public EECLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "EEC.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\24\u0139\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t"+
		" \4!\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\5"+
		"\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3"+
		"\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6"+
		"w\n\6\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\5\13\u0083\n\13\3\f\6"+
		"\f\u0086\n\f\r\f\16\f\u0087\3\f\3\f\6\f\u008c\n\f\r\f\16\f\u008d\3\f\5"+
		"\f\u0091\n\f\3\f\5\f\u0094\n\f\3\f\3\f\6\f\u0098\n\f\r\f\16\f\u0099\3"+
		"\f\5\f\u009d\n\f\3\f\5\f\u00a0\n\f\3\f\3\f\3\f\5\f\u00a5\n\f\3\f\6\f\u00a8"+
		"\n\f\r\f\16\f\u00a9\3\f\5\f\u00ad\n\f\3\f\3\f\5\f\u00b1\n\f\3\r\3\r\3"+
		"\16\3\16\5\16\u00b7\n\16\3\16\6\16\u00ba\n\16\r\16\16\16\u00bb\5\16\u00be"+
		"\n\16\3\17\3\17\3\20\3\20\7\20\u00c4\n\20\f\20\16\20\u00c7\13\20\3\20"+
		"\3\20\5\20\u00cb\n\20\3\21\3\21\3\22\3\22\3\23\3\23\5\23\u00d3\n\23\3"+
		"\24\3\24\5\24\u00d7\n\24\3\25\3\25\3\25\3\25\5\25\u00dd\n\25\3\26\3\26"+
		"\5\26\u00e1\n\26\3\26\6\26\u00e4\n\26\r\26\16\26\u00e5\3\27\3\27\3\27"+
		"\7\27\u00eb\n\27\f\27\16\27\u00ee\13\27\5\27\u00f0\n\27\3\30\3\30\5\30"+
		"\u00f4\n\30\3\31\3\31\3\32\3\32\3\32\3\32\3\32\3\32\3\32\5\32\u00ff\n"+
		"\32\3\33\3\33\3\34\3\34\3\35\3\35\3\36\3\36\3\37\3\37\6\37\u010b\n\37"+
		"\r\37\16\37\u010c\3 \3 \3!\3!\3\"\6\"\u0114\n\"\r\"\16\"\u0115\3\"\3\""+
		"\3#\6#\u011b\n#\r#\16#\u011c\3#\3#\3$\3$\3$\3$\7$\u0125\n$\f$\16$\u0128"+
		"\13$\3$\3$\3$\3$\3$\3%\3%\3%\3%\7%\u0133\n%\f%\16%\u0136\13%\3%\3%\3\u0126"+
		"\2&\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\2\33\2\35\2"+
		"\37\2!\2#\2%\2\'\2)\2+\2-\2/\2\61\2\63\2\65\2\67\29\2;\2=\16?\17A\20C"+
		"\21E\22G\23I\24\3\2\16\5\2\13\f\17\17\"\"\r\2##%%\'(,-//<<>B^^``~~\u0080"+
		"\u0080\4\2NNnn\6\2FFHHffhh\5\2&&C\\aa\4\2GGgg\4\2--//T\2C\\\u00c2\u00d8"+
		"\u00da\u00e0\u0102\u0138\u013b\u0149\u014c\u017f\u0183\u0184\u0186\u018d"+
		"\u0190\u0193\u0195\u0196\u0198\u019a\u019e\u019f\u01a1\u01a2\u01a4\u01ab"+
		"\u01ae\u01b5\u01b7\u01be\u01c6\u01cf\u01d1\u01dd\u01e0\u01f0\u01f3\u01f6"+
		"\u01f8\u01fa\u01fc\u0234\u023c\u023d\u023f\u0240\u0243\u0248\u024a\u0250"+
		"\u0372\u0374\u0378\u0381\u0388\u038c\u038e\u03a3\u03a5\u03ad\u03d1\u03d6"+
		"\u03da\u03f0\u03f6\u03f9\u03fb\u03fc\u03ff\u0431\u0462\u0482\u048c\u04cf"+
		"\u04d2\u0530\u0533\u0558\u10a2\u10c7\u10c9\u10cf\u1e02\u1e96\u1ea0\u1f00"+
		"\u1f0a\u1f11\u1f1a\u1f1f\u1f2a\u1f31\u1f3a\u1f41\u1f4a\u1f4f\u1f5b\u1f61"+
		"\u1f6a\u1f71\u1fba\u1fbd\u1fca\u1fcd\u1fda\u1fdd\u1fea\u1fee\u1ffa\u1ffd"+
		"\u2104\u2109\u210d\u210f\u2112\u2114\u2117\u211f\u2126\u212f\u2132\u2135"+
		"\u2140\u2141\u2147\u2185\u2c02\u2c30\u2c62\u2c66\u2c69\u2c72\u2c74\u2c77"+
		"\u2c80\u2c82\u2c84\u2ce4\u2ced\u2cef\u2cf4\ua642\ua644\ua66e\ua682\ua69c"+
		"\ua724\ua730\ua734\ua770\ua77b\ua788\ua78d\ua78f\ua792\ua794\ua798\ua7af"+
		"\ua7b2\ua7b3\uff23\uff3cS\2c|\u00b7\u00f8\u00fa\u0101\u0103\u0179\u017c"+
		"\u0182\u0185\u0187\u018a\u0194\u0197\u019d\u01a0\u01a3\u01a5\u01a7\u01aa"+
		"\u01af\u01b2\u01b6\u01b8\u01c1\u01c8\u01ce\u01d0\u01f5\u01f7\u01fb\u01fd"+
		"\u023b\u023e\u0244\u0249\u0295\u0297\u02b1\u0373\u0375\u0379\u037f\u0392"+
		"\u03d0\u03d2\u03d3\u03d7\u03d9\u03db\u03f5\u03f7\u0461\u0463\u0483\u048d"+
		"\u04c1\u04c4\u0531\u0563\u0589\u1d02\u1d2d\u1d6d\u1d79\u1d7b\u1d9c\u1e03"+
		"\u1e9f\u1ea1\u1f09\u1f12\u1f17\u1f22\u1f29\u1f32\u1f39\u1f42\u1f47\u1f52"+
		"\u1f59\u1f62\u1f69\u1f72\u1f7f\u1f82\u1f89\u1f92\u1f99\u1fa2\u1fa9\u1fb2"+
		"\u1fb6\u1fb8\u1fb9\u1fc0\u1fc6\u1fc8\u1fc9\u1fd2\u1fd5\u1fd8\u1fd9\u1fe2"+
		"\u1fe9\u1ff4\u1ff6\u1ff8\u1ff9\u210c\u2115\u2131\u213b\u213e\u213f\u2148"+
		"\u214b\u2150\u2186\u2c32\u2c60\u2c63\u2c6e\u2c73\u2c7d\u2c83\u2cee\u2cf0"+
		"\u2cf5\u2d02\u2d27\u2d29\u2d2f\ua643\ua66f\ua683\ua69d\ua725\ua733\ua735"+
		"\ua77a\ua77c\ua77e\ua781\ua789\ua78e\ua790\ua793\ua797\ua799\ua7ab\ua7fc"+
		"\uab5c\uab66\uab67\ufb02\ufb08\ufb15\ufb19\uff43\uff5c\b\2\u01c7\u01cd"+
		"\u01f4\u1f91\u1f9a\u1fa1\u1faa\u1fb1\u1fbe\u1fce\u1ffe\u1ffe\u00ec\2\u00ac"+
		"\u00bc\u01bd\u01c5\u0296\u05ec\u05f2\u05f4\u0622\u0641\u0643\u064c\u0670"+
		"\u0671\u0673\u06d5\u06d7\u06fe\u0701\u0712\u0714\u0731\u074f\u07a7\u07b3"+
		"\u07ec\u0802\u0817\u0842\u085a\u08a2\u08b4\u0906\u093b\u093f\u0952\u095a"+
		"\u0963\u0974\u0982\u0987\u098e\u0991\u0992\u0995\u09aa\u09ac\u09b2\u09b4"+
		"\u09bb\u09bf\u09d0\u09de\u09df\u09e1\u09e3\u09f2\u09f3\u0a07\u0a0c\u0a11"+
		"\u0a12\u0a15\u0a2a\u0a2c\u0a32\u0a34\u0a35\u0a37\u0a38\u0a3a\u0a3b\u0a5b"+
		"\u0a5e\u0a60\u0a76\u0a87\u0a8f\u0a91\u0a93\u0a95\u0aaa\u0aac\u0ab2\u0ab4"+
		"\u0ab5\u0ab7\u0abb\u0abf\u0ad2\u0ae2\u0ae3\u0b07\u0b0e\u0b11\u0b12\u0b15"+
		"\u0b2a\u0b2c\u0b32\u0b34\u0b35\u0b37\u0b3b\u0b3f\u0b63\u0b73\u0b85\u0b87"+
		"\u0b8c\u0b90\u0b92\u0b94\u0b97\u0b9b\u0b9c\u0b9e\u0bac\u0bb0\u0bbb\u0bd2"+
		"\u0c0e\u0c10\u0c12\u0c14\u0c2a\u0c2c\u0c3b\u0c3f\u0c8e\u0c90\u0c92\u0c94"+
		"\u0caa\u0cac\u0cb5\u0cb7\u0cbb\u0cbf\u0ce0\u0ce2\u0ce3\u0cf3\u0cf4\u0d07"+
		"\u0d0e\u0d10\u0d12\u0d14\u0d3c\u0d3f\u0d50\u0d62\u0d63\u0d7c\u0d81\u0d87"+
		"\u0d98\u0d9c\u0db3\u0db5\u0dbd\u0dbf\u0dc8\u0e03\u0e32\u0e34\u0e35\u0e42"+
		"\u0e47\u0e83\u0e84\u0e86\u0e8c\u0e8f\u0e99\u0e9b\u0ea1\u0ea3\u0ea5\u0ea7"+
		"\u0ea9\u0eac\u0ead\u0eaf\u0eb2\u0eb4\u0eb5\u0ebf\u0ec6\u0ede\u0ee1\u0f02"+
		"\u0f49\u0f4b\u0f6e\u0f8a\u0f8e\u1002\u102c\u1041\u1057\u105c\u105f\u1063"+
		"\u1072\u1077\u1083\u1090\u10fc\u10ff\u124a\u124c\u124f\u1252\u1258\u125a"+
		"\u125f\u1262\u128a\u128c\u128f\u1292\u12b2\u12b4\u12b7\u12ba\u12c0\u12c2"+
		"\u12c7\u12ca\u12d8\u12da\u1312\u1314\u1317\u131a\u135c\u1382\u1391\u13a2"+
		"\u13f6\u1403\u166e\u1671\u1681\u1683\u169c\u16a2\u16ec\u16f3\u16fa\u1702"+
		"\u170e\u1710\u1713\u1722\u1733\u1742\u1753\u1762\u176e\u1770\u1772\u1782"+
		"\u17b5\u17de\u1844\u1846\u1879\u1882\u18aa\u18ac\u18f7\u1902\u1920\u1952"+
		"\u196f\u1972\u1976\u1982\u19ad\u19c3\u19c9\u1a02\u1a18\u1a22\u1a56\u1b07"+
		"\u1b35\u1b47\u1b4d\u1b85\u1ba2\u1bb0\u1bb1\u1bbc\u1be7\u1c02\u1c25\u1c4f"+
		"\u1c51\u1c5c\u1c79\u1ceb\u1cee\u1cf0\u1cf3\u1cf7\u1cf8\u2137\u213a\u2d32"+
		"\u2d69\u2d82\u2d98\u2da2\u2da8\u2daa\u2db0\u2db2\u2db8\u2dba\u2dc0\u2dc2"+
		"\u2dc8\u2dca\u2dd0\u2dd2\u2dd8\u2dda\u2de0\u3008\u303e\u3043\u3098\u30a1"+
		"\u30fc\u3101\u312f\u3133\u3190\u31a2\u31bc\u31f2\u3201\u3402\u4db7\u4e02"+
		"\u9fce\ua002\ua016\ua018\ua48e\ua4d2\ua4f9\ua502\ua60d\ua612\ua621\ua62c"+
		"\ua62d\ua670\ua6e7\ua7f9\ua803\ua805\ua807\ua809\ua80c\ua80e\ua824\ua842"+
		"\ua875\ua884\ua8b5\ua8f4\ua8f9\ua8fd\ua927\ua932\ua948\ua962\ua97e\ua986"+
		"\ua9b4\ua9e2\ua9e6\ua9e9\ua9f1\ua9fc\uaa00\uaa02\uaa2a\uaa42\uaa44\uaa46"+
		"\uaa4d\uaa62\uaa71\uaa73\uaa78\uaa7c\uaab1\uaab3\uaabf\uaac2\uaac4\uaadd"+
		"\uaade\uaae2\uaaec\uaaf4\uab08\uab0b\uab10\uab13\uab18\uab22\uab28\uab2a"+
		"\uab30\uabc2\uabe4\uac02\ud7a5\ud7b2\ud7c8\ud7cd\ud7fd\uf902\ufa6f\ufa72"+
		"\ufadb\ufb1f\ufb2a\ufb2c\ufb38\ufb3a\ufb3e\ufb40\ufbb3\ufbd5\ufd3f\ufd52"+
		"\ufd91\ufd94\ufdc9\ufdf2\ufdfd\ufe72\ufe76\ufe78\ufefe\uff68\uff71\uff73"+
		"\uff9f\uffa2\uffc0\uffc4\uffc9\uffcc\uffd1\uffd4\uffd9\uffdc\uffde\4\2"+
		"\f\f\17\17\2\u0150\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13"+
		"\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2"+
		"\2\2\27\3\2\2\2\2=\3\2\2\2\2?\3\2\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2"+
		"\2G\3\2\2\2\2I\3\2\2\2\3K\3\2\2\2\5M\3\2\2\2\7O\3\2\2\2\tQ\3\2\2\2\13"+
		"v\3\2\2\2\rx\3\2\2\2\17z\3\2\2\2\21|\3\2\2\2\23~\3\2\2\2\25\u0080\3\2"+
		"\2\2\27\u00b0\3\2\2\2\31\u00b2\3\2\2\2\33\u00bd\3\2\2\2\35\u00bf\3\2\2"+
		"\2\37\u00c5\3\2\2\2!\u00cc\3\2\2\2#\u00ce\3\2\2\2%\u00d2\3\2\2\2\'\u00d6"+
		"\3\2\2\2)\u00dc\3\2\2\2+\u00de\3\2\2\2-\u00ef\3\2\2\2/\u00f3\3\2\2\2\61"+
		"\u00f5\3\2\2\2\63\u00fe\3\2\2\2\65\u0100\3\2\2\2\67\u0102\3\2\2\29\u0104"+
		"\3\2\2\2;\u0106\3\2\2\2=\u010a\3\2\2\2?\u010e\3\2\2\2A\u0110\3\2\2\2C"+
		"\u0113\3\2\2\2E\u011a\3\2\2\2G\u0120\3\2\2\2I\u012e\3\2\2\2KL\7*\2\2L"+
		"\4\3\2\2\2MN\7+\2\2N\6\3\2\2\2OP\7\60\2\2P\b\3\2\2\2QR\7o\2\2RS\7q\2\2"+
		"ST\7f\2\2TU\7w\2\2UV\7n\2\2VW\7g\2\2W\n\3\2\2\2XY\7k\2\2YZ\7p\2\2Z[\7"+
		"h\2\2[\\\7k\2\2\\w\7z\2\2]^\7k\2\2^_\7p\2\2_`\7h\2\2`a\7k\2\2ab\7z\2\2"+
		"bw\7n\2\2cd\7k\2\2de\7p\2\2ef\7h\2\2fg\7k\2\2gh\7z\2\2hw\7t\2\2ij\7r\2"+
		"\2jk\7t\2\2kl\7g\2\2lm\7h\2\2mn\7k\2\2nw\7z\2\2op\7r\2\2pq\7q\2\2qr\7"+
		"u\2\2rs\7v\2\2st\7h\2\2tu\7k\2\2uw\7z\2\2vX\3\2\2\2v]\3\2\2\2vc\3\2\2"+
		"\2vi\3\2\2\2vo\3\2\2\2w\f\3\2\2\2xy\7?\2\2y\16\3\2\2\2z{\7/\2\2{\20\3"+
		"\2\2\2|}\5\33\16\2}\22\3\2\2\2~\177\5\63\32\2\177\24\3\2\2\2\u0080\u0082"+
		"\5-\27\2\u0081\u0083\5!\21\2\u0082\u0081\3\2\2\2\u0082\u0083\3\2\2\2\u0083"+
		"\26\3\2\2\2\u0084\u0086\5/\30\2\u0085\u0084\3\2\2\2\u0086\u0087\3\2\2"+
		"\2\u0087\u0085\3\2\2\2\u0087\u0088\3\2\2\2\u0088\u0089\3\2\2\2\u0089\u008b"+
		"\7\60\2\2\u008a\u008c\5/\30\2\u008b\u008a\3\2\2\2\u008c\u008d\3\2\2\2"+
		"\u008d\u008b\3\2\2\2\u008d\u008e\3\2\2\2\u008e\u0090\3\2\2\2\u008f\u0091"+
		"\5+\26\2\u0090\u008f\3\2\2\2\u0090\u0091\3\2\2\2\u0091\u0093\3\2\2\2\u0092"+
		"\u0094\5#\22\2\u0093\u0092\3\2\2\2\u0093\u0094\3\2\2\2\u0094\u00b1\3\2"+
		"\2\2\u0095\u0097\7\60\2\2\u0096\u0098\5/\30\2\u0097\u0096\3\2\2\2\u0098"+
		"\u0099\3\2\2\2\u0099\u0097\3\2\2\2\u0099\u009a\3\2\2\2\u009a\u009c\3\2"+
		"\2\2\u009b\u009d\5+\26\2\u009c\u009b\3\2\2\2\u009c\u009d\3\2\2\2\u009d"+
		"\u009f\3\2\2\2\u009e\u00a0\5#\22\2\u009f\u009e\3\2\2\2\u009f\u00a0\3\2"+
		"\2\2\u00a0\u00b1\3\2\2\2\u00a1\u00a2\5/\30\2\u00a2\u00a4\5+\26\2\u00a3"+
		"\u00a5\5#\22\2\u00a4\u00a3\3\2\2\2\u00a4\u00a5\3\2\2\2\u00a5\u00b1\3\2"+
		"\2\2\u00a6\u00a8\5/\30\2\u00a7\u00a6\3\2\2\2\u00a8\u00a9\3\2\2\2\u00a9"+
		"\u00a7\3\2\2\2\u00a9\u00aa\3\2\2\2\u00aa\u00ac\3\2\2\2\u00ab\u00ad\5+"+
		"\26\2\u00ac\u00ab\3\2\2\2\u00ac\u00ad\3\2\2\2\u00ad\u00ae\3\2\2\2\u00ae"+
		"\u00af\5#\22\2\u00af\u00b1\3\2\2\2\u00b0\u0085\3\2\2\2\u00b0\u0095\3\2"+
		"\2\2\u00b0\u00a1\3\2\2\2\u00b0\u00a7\3\2\2\2\u00b1\30\3\2\2\2\u00b2\u00b3"+
		"\t\2\2\2\u00b3\32\3\2\2\2\u00b4\u00be\7\61\2\2\u00b5\u00b7\7\61\2\2\u00b6"+
		"\u00b5\3\2\2\2\u00b6\u00b7\3\2\2\2\u00b7\u00b9\3\2\2\2\u00b8\u00ba\5\35"+
		"\17\2\u00b9\u00b8\3\2\2\2\u00ba\u00bb\3\2\2\2\u00bb\u00b9\3\2\2\2\u00bb"+
		"\u00bc\3\2\2\2\u00bc\u00be\3\2\2\2\u00bd\u00b4\3\2\2\2\u00bd\u00b6\3\2"+
		"\2\2\u00be\34\3\2\2\2\u00bf\u00c0\t\3\2\2\u00c0\36\3\2\2\2\u00c1\u00c4"+
		"\5)\25\2\u00c2\u00c4\5/\30\2\u00c3\u00c1\3\2\2\2\u00c3\u00c2\3\2\2\2\u00c4"+
		"\u00c7\3\2\2\2\u00c5\u00c3\3\2\2\2\u00c5\u00c6\3\2\2\2\u00c6\u00ca\3\2"+
		"\2\2\u00c7\u00c5\3\2\2\2\u00c8\u00c9\7a\2\2\u00c9\u00cb\5\33\16\2\u00ca"+
		"\u00c8\3\2\2\2\u00ca\u00cb\3\2\2\2\u00cb \3\2\2\2\u00cc\u00cd\t\4\2\2"+
		"\u00cd\"\3\2\2\2\u00ce\u00cf\t\5\2\2\u00cf$\3\2\2\2\u00d0\u00d3\t\6\2"+
		"\2\u00d1\u00d3\5\65\33\2\u00d2\u00d0\3\2\2\2\u00d2\u00d1\3\2\2\2\u00d3"+
		"&\3\2\2\2\u00d4\u00d7\4c|\2\u00d5\u00d7\5\67\34\2\u00d6\u00d4\3\2\2\2"+
		"\u00d6\u00d5\3\2\2\2\u00d7(\3\2\2\2\u00d8\u00dd\5%\23\2\u00d9\u00dd\5"+
		"\'\24\2\u00da\u00dd\5;\36\2\u00db\u00dd\59\35\2\u00dc\u00d8\3\2\2\2\u00dc"+
		"\u00d9\3\2\2\2\u00dc\u00da\3\2\2\2\u00dc\u00db\3\2\2\2\u00dd*\3\2\2\2"+
		"\u00de\u00e0\t\7\2\2\u00df\u00e1\t\b\2\2\u00e0\u00df\3\2\2\2\u00e0\u00e1"+
		"\3\2\2\2\u00e1\u00e3\3\2\2\2\u00e2\u00e4\5/\30\2\u00e3\u00e2\3\2\2\2\u00e4"+
		"\u00e5\3\2\2\2\u00e5\u00e3\3\2\2\2\u00e5\u00e6\3\2\2\2\u00e6,\3\2\2\2"+
		"\u00e7\u00f0\7\62\2\2\u00e8\u00ec\5\61\31\2\u00e9\u00eb\5/\30\2\u00ea"+
		"\u00e9\3\2\2\2\u00eb\u00ee\3\2\2\2\u00ec\u00ea\3\2\2\2\u00ec\u00ed\3\2"+
		"\2\2\u00ed\u00f0\3\2\2\2\u00ee\u00ec\3\2\2\2\u00ef\u00e7\3\2\2\2\u00ef"+
		"\u00e8\3\2\2\2\u00f0.\3\2\2\2\u00f1\u00f4\7\62\2\2\u00f2\u00f4\5\61\31"+
		"\2\u00f3\u00f1\3\2\2\2\u00f3\u00f2\3\2\2\2\u00f4\60\3\2\2\2\u00f5\u00f6"+
		"\4\63;\2\u00f6\62\3\2\2\2\u00f7\u00f8\5%\23\2\u00f8\u00f9\5\37\20\2\u00f9"+
		"\u00ff\3\2\2\2\u00fa\u00fb\5\'\24\2\u00fb\u00fc\5\37\20\2\u00fc\u00ff"+
		"\3\2\2\2\u00fd\u00ff\5\33\16\2\u00fe\u00f7\3\2\2\2\u00fe\u00fa\3\2\2\2"+
		"\u00fe\u00fd\3\2\2\2\u00ff\64\3\2\2\2\u0100\u0101\t\t\2\2\u0101\66\3\2"+
		"\2\2\u0102\u0103\t\n\2\2\u01038\3\2\2\2\u0104\u0105\t\13\2\2\u0105:\3"+
		"\2\2\2\u0106\u0107\t\f\2\2\u0107<\3\2\2\2\u0108\u010b\5? \2\u0109\u010b"+
		"\5A!\2\u010a\u0108\3\2\2\2\u010a\u0109\3\2\2\2\u010b\u010c\3\2\2\2\u010c"+
		"\u010a\3\2\2\2\u010c\u010d\3\2\2\2\u010d>\3\2\2\2\u010e\u010f\7=\2\2\u010f"+
		"@\3\2\2\2\u0110\u0111\7\f\2\2\u0111B\3\2\2\2\u0112\u0114\5A!\2\u0113\u0112"+
		"\3\2\2\2\u0114\u0115\3\2\2\2\u0115\u0113\3\2\2\2\u0115\u0116\3\2\2\2\u0116"+
		"\u0117\3\2\2\2\u0117\u0118\b\"\2\2\u0118D\3\2\2\2\u0119\u011b\5\31\r\2"+
		"\u011a\u0119\3\2\2\2\u011b\u011c\3\2\2\2\u011c\u011a\3\2\2\2\u011c\u011d"+
		"\3\2\2\2\u011d\u011e\3\2\2\2\u011e\u011f\b#\2\2\u011fF\3\2\2\2\u0120\u0121"+
		"\7\61\2\2\u0121\u0122\7,\2\2\u0122\u0126\3\2\2\2\u0123\u0125\13\2\2\2"+
		"\u0124\u0123\3\2\2\2\u0125\u0128\3\2\2\2\u0126\u0127\3\2\2\2\u0126\u0124"+
		"\3\2\2\2\u0127\u0129\3\2\2\2\u0128\u0126\3\2\2\2\u0129\u012a\7,\2\2\u012a"+
		"\u012b\7\61\2\2\u012b\u012c\3\2\2\2\u012c\u012d\b$\2\2\u012dH\3\2\2\2"+
		"\u012e\u012f\7\61\2\2\u012f\u0130\7\61\2\2\u0130\u0134\3\2\2\2\u0131\u0133"+
		"\n\r\2\2\u0132\u0131\3\2\2\2\u0133\u0136\3\2\2\2\u0134\u0132\3\2\2\2\u0134"+
		"\u0135\3\2\2\2\u0135\u0137\3\2\2\2\u0136\u0134\3\2\2\2\u0137\u0138\b%"+
		"\2\2\u0138J\3\2\2\2%\2v\u0082\u0087\u008d\u0090\u0093\u0099\u009c\u009f"+
		"\u00a4\u00a9\u00ac\u00b0\u00b6\u00bb\u00bd\u00c3\u00c5\u00ca\u00d2\u00d6"+
		"\u00dc\u00e0\u00e5\u00ec\u00ef\u00f3\u00fe\u010a\u010c\u0115\u011c\u0126"+
		"\u0134\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}