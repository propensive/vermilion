package vermilion

import rudiments._

import scala.util._

trait SendmailBackend {
  def sendmail(to: Seq[String],
               from: String,
               subject: String,
               cc: Seq[String],
               bcc: Seq[String],
               bodyText: String,
               bodyHtml: Option[(String, Seq[Annex[Attachable]])],
               attachments: Seq[Annex[Attachable]]): Try[String]
}

case class Attachment(name: String, content: Array[Byte], contentType: String)

object Attachable {
  implicit val attachable: Attachable[Attachment] =
    new Attachable[Attachment] {
      def resourceName(res: Attachment): String = res.name
      def contentType(res: Attachment): String = res.contentType
      def bytes(res: Attachment): Bytes = Bytes(res.content)
    }
}

trait Attachable[Res] {
  def resourceName(res: Res): String
  def contentType(res: Res): String
  def bytes(res: Res): Bytes
}
