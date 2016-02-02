import scodec.Codec
import scodec.bits.ByteVector
import shapeless.HNil


object Main extends App {

  import ammonite.ops._

  import scodec.bits._
  import scodec.codecs.byte
  import scodec.codecs._
  import scodec.Err
  import scalaz.\/


  val bs = read.bytes(cwd/"data"/"44HB2316.FIT")

  for (b <- bs.take(10)) print(" 0x" + Integer.toHexString(b))
  println()
  val bv = ByteVector(bs)

  val fit = constant(ByteVector(".FIT".getBytes))

  case class Header(
    length: Int,
    protocolVersion: Int,
    profileVersion: Int,
    dataLength: Long,
    crc: Int)

  val header = (uint8 :: uint8 :: uint16L :: uint32L :: fit :: uint16L).as[Header]

  trait Message

  case class DefinitionMessage(
    localMessageType: LocalMessageType,
    reserved: Byte,
    architecture: Byte,
    globalMessageNumber: Int,
    fields: List[FieldDef]) extends Message

  case class FieldDef(
    number: Int,
    size: Int,
    endianAbility: Boolean,
    baseTypeNumber: Int)

  val localMessageType = byte(4)
  val fieldDefs = listOfN(uint8, (uint8 :: uint8 :: bool(1) :: ignore(2) :: uint(5)).as[FieldDef])
  val definitionMsg = (localMessageType :: byte :: byte :: uint16 :: fieldDefs).as[DefinitionMessage]

  case class DataMessage(
    localMessageType: LocalMessageType,
    content: ByteVector) extends Message

  def totalSize(definition: DefinitionMessage) =
    definition.fields.map(_.size).sum

  type LocalMessageType = Byte
  def dataMessage(defs: Map[LocalMessageType, DefinitionMessage]) = {
    val init = discriminated[DataMessage].by(byte(4).as[Byte])
    defs.foldLeft(init) { (codec, defs) =>
      val (localMessageType, definitionMessage) = defs
      val size = totalSize(definitionMessage)
      val messageOfSize = bytes(size).xmap[DataMessage](
        bv => DataMessage(localMessageType, bv),
        dm => dm.content)
      codec.typecase(localMessageType, messageOfSize)
    }
  }
  
  val rec = discriminated[Message]
            .by(byte(4).as[Byte])
            .typecase(0x4, definitionMsg)
            .typecase(0x0, fail(Err("not implemented")))

  case class File(
    header: Header,
    records: Message)

  val file = (header :: rec).as[File]

  val decoded = file.decode(bv.bits)
  println(decoded)
}
