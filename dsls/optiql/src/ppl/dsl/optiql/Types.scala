package ppl.dsl.optiql
 
trait Types {
  this: OptiQL =>

  abstract class DataTable[T] extends Record

  class LineItem (
    val orderKey: Int,
    val partKey: Int,
    val supplierKey: Int,
    val lineNumber: Int,
    val quantity: Float,
    val extendedPrice: Float,
    val discount: Float,
    val tax: Float,
    val returnFlag: Char,
    val lineStatus: Char,
    val shipDate: Date,
    val commitDate: Date,
    val receiptDate: Date,
    val shipInstruct: String,
    val shipMode: String,
    val comment: String
  )
  
  type LineItemTable = DataTable[LineItem]

  class Customer (
    val c_custkey: Int,
    val c_name: String,
    val c_address: String,
    val c_nationkey: Int,
    val c_phone: String,
    val c_acctbal: Double,
    val c_mktsegment: String,
    val c_comment: String
  )

  type CustomerTable = DataTable[Customer]

  class Nation (
    val n_nationkey: Int,
    val n_name: String,
    val n_regionkey: Int,
    val n_comment: String
  )

  type NationTable = DataTable[Nation]

  class Order (
    val o_orderkey: Int,
    val o_custkey: Int,
    val o_orderstatus: Char,
    val o_totalprice: Double,
    val o_orderdate: Date,
    val o_orderpriority: String,
    val o_clerk: String,
    val o_shippriority: Int,
    val o_comment: String
  )

  type OrderTable = DataTable[Order]

  class Part(
    val p_partkey: Int,
    val p_name: String,
    val p_mfgr: String,
    val p_brand: String,
    val p_type: String,
    val p_size: Int,
    val p_container: String,
    val p_retailprice: Double,
    val p_comment:String
  )

  type PartTable = DataTable[Part]

  class PartSupplier (
    val ps_partkey: Int,
    val ps_suppkey: Int,
    val ps_availqty: Int,
    val ps_supplycost: Double,
    val ps_comment: String
  )

  type PartSupplierTable = DataTable[PartSupplier]

  class Region (
    val r_regionkey: Int,
    val r_name: String,
    val r_comment: String
  )

  type RegionTable = DataTable[Region]

  class Supplier(
    val s_suppkey: Int,
    val s_name: String,
    val s_address: String,
    val s_nationkey: Int,
    val s_phone: String,
    val s_acctbal: Double,
    val s_comment: String
  )

  type SupplierTable = DataTable[Supplier]

  class Date(val year: Int, val month: Int, val day: Int)

}
