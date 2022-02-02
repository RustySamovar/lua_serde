use std::fs;
use std::io::Read;
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::result;

use serde::Deserialize;
use serde::ser::{self};
use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
    VariantAccess, Visitor, Error, DeserializeOwned
};

use rlua::{Function, Table, Lua, MetaMethod, UserData, UserDataMethods, Variadic, Value, TablePairs};

/*
 * Stolen from SO: https://stackoverflow.com/a/69324393
 */
macro_rules! cast {
        ($target: expr, $pat: path) => {
            {
                if let $pat(a) = $target { // #1
                    a
                } else {
                    panic!(
                        "mismatch variant {:?} cast to {}",
                        $target,
                        stringify!($pat)); // #2
                }
            }
        };
    }

pub type Result<T> = result::Result<T, LuaDeserError>;

#[derive(Clone, Debug, PartialEq)]
pub enum LuaDeserError {
    Message(String),
}

impl ser::Error for LuaDeserError {
    fn custom<T: Display>(msg: T) -> Self {
        LuaDeserError::Message(msg.to_string())
    }
}

impl de::Error for LuaDeserError {
    fn custom<T: Display>(msg: T) -> Self {
        LuaDeserError::Message(msg.to_string())
    }
}

impl Display for LuaDeserError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LuaDeserError::Message(msg) => formatter.write_str(msg),
            _ => formatter.write_str("unexpected end of input"),
            /* and so forth */
        }
    }
}

impl From<rlua::Error> for LuaDeserError {
    fn from(a: rlua::Error) -> Self {
        unimplemented!()
    }
}

impl std::error::Error for LuaDeserError {}

pub struct Deserializer<'lua> {
    input: Value<'lua>,
}

impl<'de,'lua> Deserializer<'lua> {
    pub fn from_value(input: Value<'lua>) -> Self {
        Deserializer {
            input
        }
    }

    fn deserialize_table<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }
}

struct LuaTableHelper<'lua> {
    pairs: TablePairs<'lua, Value<'lua>, Value<'lua>>,
    last_value: Option<Value<'lua>>,
}

impl<'lua> LuaTableHelper<'lua> {
    fn new(pairs: TablePairs<'lua, Value<'lua>, Value<'lua>>) -> Self {
        LuaTableHelper {
            pairs,
            last_value: None,
        }
    }
}

impl<'de, 'lua> MapAccess<'de> for LuaTableHelper<'lua> {
    type Error = LuaDeserError;

    fn next_key_seed<R>(&mut self, seed: R) -> Result<Option<R::Value>>
    where
        R: DeserializeSeed<'de>,
    {
        match self.pairs.next() {
            Some(item) => {
                let (key, value) = item?;
                self.last_value = Some(value);
                //let key_de = Deserializer { input: key };
                //seed.deserialize(key_de).map(Some)
                let mut deserializer = Deserializer::from_value(key);
                seed.deserialize(deserializer).map(Some)
            },
            None => Ok(None),
        }
    }

    fn next_value_seed<S>(&mut self, seed: S) -> Result<S::Value>
    where
        S: DeserializeSeed<'de>,
    {
            //let mut deserializer = Deserializer::from_value(&v);
            //seed.deserialize(&mut deserializer)
        match self.last_value.take() {
            Some(value) => {
                let mut deserializer = Deserializer::from_value(value);
                seed.deserialize(deserializer)
                //seed.deserialize(Deserializer { input: value })
            },
            None => Err(LuaDeserError::custom("Trying to get value from the end of the map!")),
        }
    }
}

pub fn from_value<'a, 'lua, T>(s: Value<'lua>) -> Result<T>
where
    T: Deserialize<'a>,
    //T: DeserializeOwned
{
    let mut deserializer = Deserializer::from_value(s);
    let t = T::deserialize(deserializer)?;
    return Ok(t);
    /*
    if deserializer.input.is_empty() {
        Ok(t)
    } else {
        Err(Error::TrailingCharacters)
    }
    */
}

impl<'de, 'lua> de::Deserializer<'de> for Deserializer<'lua> {
    type Error = LuaDeserError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.input {
            Value::Nil => self.deserialize_unit(visitor),
            Value::Boolean(b) => visitor.visit_bool(b),
            Value::<'lua>::String(s) => visitor.visit_str(s.to_str()?),
            Value::Integer(i) => visitor.visit_i64(i),
            Value::Number(n) => visitor.visit_f64(n),
            //Value::Table(t) => self.deserialize_table(visitor),
            Value::Function(f) => visitor.visit_str("Unimplemented: function"),
            Value::Table(t) => visitor.visit_str("Unimplemented: table"),
            _ => Err(Self::Error::custom(format!("Syntax error: unhandled type of {:?}", self.input))),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(cast!(self.input, Value::<'lua>::Boolean))
    }

    // The `parse_signed` function is generic over the integer type `T` so here
    // it is invoked with `T=i8`. The next 8 methods are similar.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i8(cast!(self.input, Value::Integer) as i8)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i16(cast!(self.input, Value::Integer) as i16)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i32(cast!(self.input, Value::Integer) as i32)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i64(cast!(self.input, Value::Integer) as i64)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u8(cast!(self.input, Value::Integer) as u8)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u16(cast!(self.input, Value::Integer) as u16)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u32(cast!(self.input, Value::Integer) as u32)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u64(cast!(self.input, Value::Integer) as u64)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        //visitor.visit_f32(cast!(self.input, Value::Number) as f32)
        // Sometimes float can be represented as integer, so custom code here
        let value = match self.input {
            Value::Integer(i) => i as f32,
            Value::Number(f) => f as f32,
            _ => panic!("Unknown floating value {:?}", self.input),
        };
        visitor.visit_f32(value)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f64(cast!(self.input, Value::Number) as f64)
    }

    // The `Serializer` implementation on the previous page serialized chars as
    // single-character strings so handle that representation here.
    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parse a string, check that it is one character, call `visit_char`.
        unimplemented!()
    }

    // Refer to the "Understanding deserializer lifetimes" page for information
    // about the three deserialization flavors of strings in Serde.
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_str(cast!(self.input, Value::<'lua>::String).to_str()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    // The `Serializer` implementation on the previous page serialized byte
    // arrays as JSON arrays of bytes. Handle that representation here.
    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.input {
            Value::Nil => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.input {
            Value::Nil => visitor.visit_unit(),
            _ => Err(Self::Error::custom(format!("Expected Nil, found {:?}", self.input))),
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    // Deserialization of compound types like sequences and maps happens by
    // passing the visitor an "Access" object that gives it the ability to
    // iterate through the data contained in the sequence.
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parse the opening bracket of the sequence.
        /*
        if self.next_char()? == '[' {
            // Give the visitor access to each element of the sequence.
            let value = visitor.visit_seq(CommaSeparated::new(self))?;
            // Parse the closing bracket of the sequence.
            if self.next_char()? == ']' {
                Ok(value)
            } else {
                Err(Error::ExpectedArrayEnd)
            }
        } else {
            Err(Error::ExpectedArray)
        }
        */
        unimplemented!()
    }

    // Tuples look just like sequences in JSON. Some formats may be able to
    // represent tuples more efficiently.
    //
    // As indicated by the length parameter, the `Deserialize` implementation
    // for a tuple in the Serde data model is required to know the length of the
    // tuple before even looking at the input data.
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // Tuple structs look just like sequences in JSON.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // Much like `deserialize_seq` but calls the visitors `visit_map` method
    // with a `MapAccess` implementation, rather than the visitor's `visit_seq`
    // method with a `SeqAccess` implementation.
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let map = cast!(self.input, Value::Table);
        let value = visitor.visit_map(LuaTableHelper::new(map.pairs()))?;
        return Ok(value);
        // Parse the opening brace of the map.
        /*
        if self.next_char()? == '{' {
            // Give the visitor access to each entry of the map.
            let value = visitor.visit_map(CommaSeparated::new(self))?;
            // Parse the closing brace of the map.
            if self.next_char()? == '}' {
                Ok(value)
            } else {
                Err(Error::ExpectedMapEnd)
            }
        } else {
            Err(Error::ExpectedMap)
        }
        */
        //unimplemented!()
    }

    // Structs look just like maps in JSON.
    //
    // Notice the `fields` parameter - a "struct" in the Serde data model means
    // that the `Deserialize` implementation is required to know what the fields
    // are before even looking at the input data. Any key-value pairing in which
    // the fields cannot be known ahead of time is probably a map.
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        /*
        if self.peek_char()? == '"' {
            // Visit a unit variant.
            visitor.visit_enum(self.parse_string()?.into_deserializer())
        } else if self.next_char()? == '{' {
            // Visit a newtype variant, tuple variant, or struct variant.
            let value = visitor.visit_enum(Enum::new(self))?;
            // Parse the matching close brace.
            if self.next_char()? == '}' {
                Ok(value)
            } else {
                Err(Error::ExpectedMapEnd)
            }
        } else {
            Err(Error::ExpectedEnum)
        }*/
        unimplemented!()
    }

    // An identifier in Serde is the type that identifies a field of a struct or
    // the variant of an enum. In JSON, struct fields and enum variants are
    // represented as strings. In other formats they may be represented as
    // numeric indices.
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    // Like `deserialize_any` but indicates to the `Deserializer` that it makes
    // no difference which `Visitor` method is called because the data is
    // ignored.
    //
    // Some deserializers are able to implement this more efficiently than
    // `deserialize_any`, for example by rapidly skipping over matched
    // delimiters without paying close attention to the data in between.
    //
    // Some formats are not able to implement this at all. Formats that can
    // implement `deserialize_any` and `deserialize_ignored_any` are known as
    // self-describing.
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

pub fn from_ctx<'a, T>(lua_ctx: &mut rlua::Context, buffer: &[u8]) -> T
where T : Deserialize<'a>
{
    let globals = lua_ctx.globals();

    let excluded: HashSet<String> = globals.pairs::<Value,Value>()
        .into_iter()
        .map(|p| p.unwrap().0)
        .filter_map(|x| match x { Value::String(s) => Some(s), _ => None})
        .map(|s| s.to_str().unwrap().to_string())
        .collect();

    lua_ctx.load(buffer).exec();

    let globals = lua_ctx.globals();
    let G = globals.get("_G").unwrap();
    let val: T = from_value(G).unwrap();
    val
}

pub fn from_file<'a, T>(filename: &str) -> result::Result<T, std::io::Error>
where T : Deserialize<'a>
{
    println!("Filename: {}", filename);

    let mut f = fs::File::open(&filename)?;
    let metadata = fs::metadata(&filename)?;
    let mut buffer = vec![0; metadata.len() as usize];
    f.read(&mut buffer)?;

    let lua = Lua::new();

    let val = lua.context(|mut lua_ctx| { return from_ctx(&mut lua_ctx, &buffer); });

    Ok(val)
}

pub fn lua_print(a: Value, offset: usize) {
    match a {
        Value::String(s) => print!("{}", s.to_str().unwrap()),
        Value::Number(n) => print!("{}", n),
        Value::Integer(i) => print!("{}", i),
        Value::Boolean(b) => print!("{}", b),
        Value::Table(t) => {println!("Table:"); lua_print_table(t, &HashSet::new(), offset+1);},
        Value::Function(f) => println!("{:?}", f),
        _ => panic!("Unsupported {:?}", a),
    }
}

pub fn lua_to_string(a: &Value) -> String {
    match a {
        Value::String(s) => return s.to_str().unwrap().to_string(),
        Value::Integer(i) => return format!("{}", i),
        _ => panic!("Unsupported {:?}", a),
    }
}

pub fn lua_print_table(t: Table, excluded: &HashSet<String>, offset: usize) {
    for pair in t.pairs::<Value, Value>() {
        let (key, value) = pair.unwrap();
        if !excluded.contains(&lua_to_string(&key)) {
            print!("{:offset$}", "", offset = offset*4);
            print!("Key: ");
            lua_print(key, offset);
            print!(", Value: ");
            lua_print(value, offset);
            println!("");
        }
    }
}
