package vermilion

import com.sendgrid._, helpers.mail._, objects._

import rudiments.*

import scala.util.*

import java.io.IOException

case class SendException() extends RuntimeException("the email could not be sent")
case class SendAddressException() extends RuntimeException("the email could not be sent")

case class MailtoUri(email: String):
  override def toString = s"mailto:$email"

case class HtmlEmail(html: String, inlines: List[Annex[Attachable]],
                         attachments: List[Annex[Attachable]])


case class Contact(email: String, name: Option[String] = None):
  override def toString = if (name == None) email else s""""${name.get}" <${email}>"""

trait Mailable[T]:
  def bodyText(t: T): String
  def bodyHtml(t: T): Option[(String, Seq[Annex[Attachable]])]
  def attachments(t: T): Seq[Annex[Attachable]]

object Mailable:
  implicit val stringMailable: Mailable[String] = new Mailable[String]:
    def bodyText(t: String) = t
    def bodyHtml(t: String) = None
    def attachments(t: String): Seq[Annex[Attachable]] = Nil

  implicit def htmlMailable(implicit conv: HtmlToPlainTextConverter): Mailable[HtmlEmail] =
    new Mailable[HtmlEmail]:
      def bodyText(t: HtmlEmail) = conv.convert(t.html)
      def bodyHtml(t: HtmlEmail) = Some((t.html, t.inlines))
      def attachments(t: HtmlEmail) = t.attachments

object EmailMessage:
  implicit val emailMessageSendable: Sendable[EmailMessage] = new Sendable[EmailMessage]:
    def to(env: EmailMessage): Seq[Contact] = env.to
    def cc(env: EmailMessage): Seq[Contact] = env.cc
    def bcc(env: EmailMessage): Seq[Contact] = env.bcc
    def from(env: EmailMessage): Contact = env.from
    def subject(env: EmailMessage): String = env.subject
    def bodyText(env: EmailMessage): String = env.mailable(_.bodyText)
    def bodyHtml(env: EmailMessage): Option[(String, Seq[Annex[Attachable]])] = env.mailable(_.bodyHtml)
    def attachments(env: EmailMessage): Seq[Annex[Attachable]] = env.mailable(_.attachments)

case class EmailMessage(from: Contact, to: Seq[Contact], cc: Seq[Contact], bcc: Seq[Contact],
                            subject: String, mailable: Annex[Mailable],
                            attachments: Seq[Annex[Attachable]]*)

object Sendable:
  case class Capability[T: Sendable](msg: T):
    def send()(implicit sendmailBackend: SendmailBackend): Try[String] =
      sendmailBackend.sendmail(
        to = implicitly[Sendable[T]].to(msg).map(_.toString),
        from = implicitly[Sendable[T]].from(msg).toString,
        subject = implicitly[Sendable[T]].subject(msg),
        cc = implicitly[Sendable[T]].cc(msg).map(_.toString),
        bcc = implicitly[Sendable[T]].bcc(msg).map(_.toString),
        bodyText = implicitly[Sendable[T]].bodyText(msg),
        bodyHtml = implicitly[Sendable[T]].bodyHtml(msg),
        attachments = implicitly[Sendable[T]].attachments(msg)
      )

trait Sendable[T]:
  def to(t: T): Seq[Contact]
  def cc(t: T): Seq[Contact]
  def bcc(t: T): Seq[Contact]
  def from(t: T): Contact
  def subject(t: T): String
  def bodyText(t: T): String
  def bodyHtml(t: T): Option[(String, Seq[Annex[Attachable]])]
  def attachments(t: T): Seq[Annex[Attachable]]

case class Envelope(subject: String, from: Contact, to: Seq[Contact], cc: Seq[Contact] = Nil,
                        bcc: Seq[Contact] = Nil):
  def insert(mailable: Annex[Mailable], attachments: Annex[Attachable]*): EmailMessage =
    EmailMessage(from, to, cc, bcc, subject, mailable, attachments)

object sendgridBackend extends SendmailBackend:

  def extract(email: String): (String, String) = email.trim.split("<", 2) match
    case Array(name, e) => (name.trim, e.trim.dropRight(1))
    case Array(e) => ("", e)

  def base64(data: Array[Byte]): String = String(java.util.Base64.getEncoder().encode(data))

  def sendmail(to: Seq[String], from: String, subject: String, cc: Seq[String], bcc: Seq[String],
                   bodyText: String, bodyHtml: Option[(String, Seq[Annex[Attachable]])],
                   attachments: Seq[Annex[Attachable]]): Try[String] = Try {

    val mail = new Mail()

    val fromEmail = new Email()
    val fromContact = extract(from)
    fromEmail.setName(fromContact.at[0].replaceAll("\"", ""))
    fromEmail.setEmail(fromContact.at[1])
    mail.setFrom(fromEmail)

    mail.setSubject(subject)

    val personalization = new Personalization()
    to.map(extract).foreach { (toName, toEmail) =>
      val to = new Email()
      to.setName(toName)
      to.setEmail(toEmail)
      personalization.addTo(to)
    }

    cc.map(extract).foreach { (ccName, ccEmail) =>
      val cc = new Email()
      cc.setName(ccName)
      cc.setEmail(ccEmail)
      personalization.addCc(cc)
    }

    personalization.setSubject(subject)
    mail.addPersonalization(personalization)

    val content = new Content()
    content.setType("text/plain")
    content.setValue(bodyText)
    mail.addContent(content)
   
    bodyHtml.foreach { case (html, inlines) =>
      content.setType("text/html")
      content.setValue(html)
      mail.addContent(content)
   
      inlines.foreach { inline =>
        val atts = new Attachments()
        atts.setContent(base64(inline(_.bytes.to[Array] _)))
        atts.setType(inline(_.contentType _))
        atts.setFilename(inline(_.resourceName _))
        atts.setDisposition("inline")
        atts.setContentId(inline(_.resourceName _))
        mail.addAttachments(atts)
      }
    }

    attachments.foreach { att =>
      val atts = new Attachments()
      atts.setContent(base64(att(_.bytes.to[Array] _)))
      atts.setType(att(_.contentType _))
      atts.setFilename(att(_.resourceName _))
      atts.setDisposition("attachment")
      atts.setContentId(att(_.resourceName _))
      mail.addAttachments(atts)
    }

    val sg = new SendGrid("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")

    val request = new com.sendgrid.Request()
    try
      request.setMethod(com.sendgrid.Method.POST)
      request.setEndpoint("mail/send")
      request.setBody(mail.build())
      val response = sg.api(request)
      println(response.getStatusCode())
      println(response.getBody())
      println(response.getHeaders())
      response.getStatusCode().toString
    catch case ex: IOException => throw ex
  }
}

object htmlToPlainTextConverters:

  object plainText:
    implicit val htmlToPlainTextConverter: HtmlToPlainTextConverter = new HtmlToPlainTextConverter:
      def convert(html: String): String = html.replaceAll("<[^>]>", "")

  object elinks:
    implicit val htmlToPlainTextConverter: HtmlToPlainTextConverter = new HtmlToPlainTextConverter:
      def convert(html: String): String =
        val file = new java.io.File(s"/tmp/${java.util.UUID.randomUUID().toString}.html")
        val fw = new java.io.FileWriter(file)
        fw.write(html)
        fw.close()
        implicit val env = environments.enclosing.copy(workDir = Some(file.getParent))
        val output = sh"elinks -dump -dump-width 76 '${file.getAbsolutePath}'".exec[String]
        file.delete()
        output

  object links:
    implicit val htmlToPlainTextConverter: HtmlToPlainTextConverter = new HtmlToPlainTextConverter:
      def convert(html: String): String =
        val file = new java.io.File(s"/tmp/${java.util.UUID.randomUUID().toString}.html")
        val fw = new java.io.FileWriter(file)
        fw.write(html)
        fw.close()
        implicit val env = environments.enclosing.copy(workDir = Some(file.getParent))
        val output = sh"links -dump 'file://${file.getAbsolutePath}'".exec[String]
        file.delete()
        output

  object lynx:
    implicit val htmlToPlainTextConverter: HtmlToPlainTextConverter = new HtmlToPlainTextConverter
      def convert(html: String): String =
        val file = new java.io.File(s"/tmp/${java.util.UUID.randomUUID().toString}.html")
        val fw = new java.io.FileWriter(file)
        fw.write(html)
        fw.close()
        implicit val env = environments.enclosing.copy(workDir = Some(file.getParent))
        val output = sh"lynx -dump -width=76 'file://${file.getAbsolutePath}'".exec[String]
        file.delete()
        output

trait HtmlToPlainTextConverter:
  def convert(html: String): String
