package wowchat.discord

import java.awt.Color
import java.time.Instant
import java.util.regex.Pattern

import wowchat.commands.CommandHandler
import wowchat.common._
import com.typesafe.scalalogging.StrictLogging
import com.vdurmont.emoji.EmojiParser
import net.dv8tion.jda.api.{JDABuilder, Permission}
import net.dv8tion.jda.api.JDA.Status
import net.dv8tion.jda.api.entities.{Activity, ChannelType, Emote, Guild, MessageType, Role}
import net.dv8tion.jda.api.entities.Activity.ActivityType
import net.dv8tion.jda.api.events.guild.GuildReadyEvent
import net.dv8tion.jda.api.events.interaction.SlashCommandEvent
import net.dv8tion.jda.api.events.{ShutdownEvent, StatusChangeEvent}
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter
import net.dv8tion.jda.api.interactions.commands.OptionType
import net.dv8tion.jda.api.requests.{CloseCode, GatewayIntent}
import net.dv8tion.jda.api.utils.MemberCachePolicy
import net.dv8tion.jda.api.utils.cache.CacheFlag
import wowchat.game.GamePackets

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Discord(discordConnectionCallback: CommonConnectionCallback) extends ListenerAdapter
  with GamePackets with StrictLogging {

  private val jda = JDABuilder
    .createDefault(Global.config.discord.token, GatewayIntent.GUILD_MESSAGES, GatewayIntent.GUILD_MEMBERS, GatewayIntent.GUILD_PRESENCES, GatewayIntent.GUILD_EMOJIS)
    .setMemberCachePolicy(MemberCachePolicy.ALL)
    .disableCache(CacheFlag.VOICE_STATE)
    .addEventListeners(this)
    .build

  private val messageResolver = MessageResolver(jda)

  private var lastStatus: Option[Activity] = None
  private var firstConnect = true

  private final val MENTION_TIMEOUT_SEC = 60 * 10  // 10min.
  private val mentionTimeouts = collection.mutable.Map[String, ListBuffer[String]]()

  class TagRole(var raidName: String, var color: Color, var raidSize: Int = 0, var tagName: String = "", var regex: String = "")
  {
    if(tagName.isEmpty) {
      // Default to "LFM_{name}{size}" (LFM_NAXX25)
      val sizeName = if(raidSize != 0) raidSize.toString else ""
      tagName = "LFM_" + raidName + sizeName
    }

    if(regex.isEmpty) {
      // Default to "LF*M_;_{raid}#;{size}"
      val sizePattern = if(raidSize != 0) ";" + raidSize.toString else ""
      regex = "LF*M_;_" + raidName + "#" + sizePattern
    }
  }

  /*
   * _ is a required space in the message
   * ; separates required keywords/regexes
   * * Any character or nothing
   * # Number or space
  */
  private val roles = List(
    new TagRole("Naxx", Color.decode("#1f8b4c")),
    new TagRole("Naxx", Color.decode("#1f8b4c"), 10),
    new TagRole("Naxx", Color.decode("#1f8b4c"), 25),
    new TagRole("Uld",  Color.decode("#00cedd")),
    new TagRole("Uld",  Color.decode("#00cedd"), 10),
    new TagRole("Uld",  Color.decode("#00cedd"), 25),
    new TagRole("ToC",  Color.decode("#e67e22")),
    new TagRole("ToC",  Color.decode("#e67e22"), 10),
    new TagRole("ToC",  Color.decode("#e67e22"), 25),
    new TagRole("ToGC", Color.decode("#a84300")),
    new TagRole("ToGC", Color.decode("#a84300"), 10),
    new TagRole("ToGC", Color.decode("#a84300"), 25),
    new TagRole("ICC",  Color.decode("#206694")),
    new TagRole("ICC",  Color.decode("#206694"), 10),
    new TagRole("ICC",  Color.decode("#206694"), 25),
    new TagRole("RS",   Color.decode("#e91e63")),
    new TagRole("RS",   Color.decode("#e91e63"), 10),
    new TagRole("RS",   Color.decode("#e91e63"), 25),
    new TagRole("Ony",  Color.decode("#992d22")),
    new TagRole("Ony",  Color.decode("#992d22"), 10),
    new TagRole("Ony",  Color.decode("#992d22"), 25),
    new TagRole("VoA",  Color.decode("#9b59b6")),
    new TagRole("VoA",  Color.decode("#9b59b6"), 10),
    new TagRole("VoA",  Color.decode("#9b59b6"), 25)
  )

  def changeStatus(gameType: ActivityType, message: String): Unit = {
    lastStatus = Some(Activity.of(gameType, message))
    jda.getPresence.setActivity(lastStatus.get)
  }

  def changeGuildStatus(message: String): Unit = {
    changeStatus(ActivityType.WATCHING, message)
  }

  def changeRealmStatus(message: String): Unit = {
    changeStatus(ActivityType.DEFAULT, message)
  }

  def sendMessageFromWow(from: Option[String], message: String, wowType: Byte, wowChannel: Option[String]): Unit = {
    Global.wowToDiscord.get((wowType, wowChannel.map(_.toLowerCase))).foreach(discordChannels => {
      var formattedMessage =
        messageResolver.stripColorCoding(
        escapeFormatting(
        messageResolver.resolveLinks(message)))

      //Message without raid icons, used when searching for role tagging patterns.
      val cleanMessage = removeRaidIcons(formattedMessage).toLowerCase

      discordChannels.foreach {
        case (channel, channelConfig) =>
          val mentions = getRoleMentions(cleanMessage, from, channel.getGuild)

          formattedMessage = raidIconsToEmotes(channel.getGuild, formattedMessage) //Convert raid icons ({skull}, {star} etc)
          formattedMessage = thunderfury(channel.getGuild, formattedMessage) //Did someone say [Thunderfury, Blessed Blade of the Windseeker]?
          formattedMessage += "\n" + mentions

          var errors = mutable.ArrayBuffer.empty[String]
          val parsedResolvedTags = from.map(_ => {
            messageResolver.resolveTags(channel, formattedMessage, errors += _)
          }).getOrElse(formattedMessage)

          val formatted = channelConfig
            .format
            .replace("%time", Global.getTime)
            .replace("%user", from.getOrElse(""))
            .replace("%message", parsedResolvedTags)
            .replace("%target", wowChannel.getOrElse(""))

          val filter = shouldFilter(channelConfig.filters, formatted)
          logger.info(s"${if (filter) "FILTERED " else ""}WoW->Discord(${channel.getName}) $formatted")
          if (!filter) {
            channel.sendMessage(formatted).queue()
          }
          if (Global.config.discord.enableTagFailedNotifications) {
            errors.foreach(error => {
              Global.game.foreach(_.sendMessageToWow(ChatEvents.CHAT_MSG_WHISPER, error, from))
              channel.sendMessage(error).queue()
            })
          }
      }
    })
  }

  def escapeFormatting(msg: String) : String = {
    //Escape any formatting. "|" in the chat appears as "||" here, so replace "||" with an escaped "|"
    msg
      .replace("@", "@\u200B")
      .replace("_", "\\_")
      .replace("*", "\\*")
      .replace("`", "\\`")
      .replace("||", "\\|")
  }

  def thunderfury(guild: Guild, message: String) : String = {
    if(!message.contains("[Thunderfury, Blessed Blade of the Windseeker]")) return message
    val mention = getEmoteMention(guild, "thunderfury")
    return message
        .replace("[Thunderfury, Blessed Blade of the Windseeker]", mention+mention+mention)
        .replace("(http://wotlk-twinhead.twinstar.cz?item=19019)", "")
  }

  def checkRequiredDiscordRoles(guild: Guild): Unit =
  {
    roles.foreach(role =>
    {
      val reqRoles = guild.getRolesByName(role.tagName, true)
      if(reqRoles.isEmpty)
        {
          guild.createRole()
              .setName(role.tagName)
              .setColor(role.color)
              .setHoisted(false)
              .setMentionable(true)
              .setPermissions()
              .queue()

          logger.info("Created new role for " + role.tagName + ".")
        }
    })
  }

  def getRoleMentions(cleanMessage: String, sender: Option[String], guild: Guild) : String = {
    val rolesBuilder = new StringBuilder()

    var targetRaid = ""  // Restrict mentions to one raid/message.
    roles.foreach(role =>
    {
      if(!targetRaid.isEmpty && role.raidName != targetRaid) return rolesBuilder.toString()

      val roleRegex = role.regex.replace("_", " ").toLowerCase

      val roles = guild.getRolesByName(role.tagName, true)

      if(!roles.isEmpty)
        {
          val serverRole = guild.getRolesByName(role.tagName, true).get(0)

          //Split tag by ';' so we can have multiple requirements for one tag e.g. LF*M;ICC;
          val split = roleRegex.split(";")
          var valid = true
          split.foreach(tag =>
          {
            //Replace * with .? regex (match zero or one characters of any type, good for matching LFM/LF1M/LF2M with LF*M)
            //Replace # with [0-9 ] regex (Match numeral or space)
            val pattern = tag.replace("*", ".?").replace("#", "[0-9 ]").r
            if(pattern.findFirstIn(cleanMessage).isEmpty)
            {
              valid = false
            }
          })

          if(valid)
          {
            val str = if(rolesBuilder.length() == 0) "" else " "
            if(sender.isDefined)
            {
              if(hasTagTimeout(sender.get, serverRole.getName))
              {
                rolesBuilder.append(str + serverRole.getAsMention)
              }
              else
              {
                // Display timed-out mention in parentheses; nice for debugging.
                // rolesBuilder.append(str + "(" + escapeFormatting(serverRole.getName) + ")")
              }
            }
            targetRaid = role.raidName
          }
        }
    })
    return rolesBuilder.toString()
  }

  val icons = Map( //Wow raid icon -> discord emote
    "{star}"     -> "rt1",
    "{circle}"   -> "rt2",
    "{coin}"     -> "rt2",
    "{diamond}"  -> "rt3",
    "{triangle}" -> "rt4",
    "{moon}"     -> "rt5",
    "{square}"   -> "rt6",
    "{cross}"    -> "rt7",
    "{x}"        -> "rt7",
    "{skull}"    -> "rt8",

    "{rt1}" -> "rt1",
    "{rt2}" -> "rt2",
    "{rt3}" -> "rt3",
    "{rt4}" -> "rt4",
    "{rt5}" -> "rt5",
    "{rt6}" -> "rt6",
    "{rt7}" -> "rt7",
    "{rt8}" -> "rt8")

  def removeRaidIcons(message: String) : String = {
    var finalMessage = message
    icons.keys.foreach(icon =>
    {
      finalMessage = finalMessage.replaceAll("(?i)" + Pattern.quote(icon), "")
    })
    finalMessage
  }

  def raidIconsToEmotes(guild: Guild, message: String) : String = {
    var finalMessage = message
    icons.keys.foreach(icon =>
    {
      finalMessage = finalMessage.replaceAll("(?i)" + Pattern.quote(icon), getEmoteMention(guild, icons.apply(icon)))
    })
    return finalMessage
  }

  def getEmoteMention(guild: Guild, emote: String) : String = {
    val emotes = guild.getEmotesByName(emote, true)
    if(emotes.size > 0)
    {
      return emotes.get(0).getAsMention
    }
    return ""
  }

  def hasTagTimeout(sender: String, role: String) : Boolean = { //Returns true if the user is allowed to tag the role
    val timeNow = Instant.now.getEpochSecond
    cleanTimeouts()
    mentionTimeouts.get(sender) match {
      case Some(_) =>
        if(getUserTagTimeout(sender, role) != -1) {
          return false //User has a timeout.
        }
      case None => //No data found for user, timeout not set
    }

    addUserTagTimeout(sender, role) //User doesn't have timeout for this role yet, set it
    return true
  }

  def addUserTagTimeout(user: String, role: String) : Unit = {
    if(getUserTagTimeout(user, role) != -1) return //Timeout already exists

    if(!mentionTimeouts.contains(user))
    {
      val newList = new ListBuffer[String]()
      mentionTimeouts += (user -> newList)
    }
    val timeNow = Instant.now.getEpochSecond
    mentionTimeouts.apply(user) += role + ":::" + timeNow
  }

  def getUserTagTimeout(sender: String, role: String) : Int = {
    if(!mentionTimeouts.contains(sender)) return -1

    val timeouts = mentionTimeouts.apply(sender)
    val timeNow = Instant.now.getEpochSecond
    timeouts.foreach(timeout =>
    {
      val split = timeout.split(":::")
      if(split(0) == role) return split(1).toInt
    })
    return -1
  }

  def cleanTimeouts() : Unit = {
    val timeNow = Instant.now.getEpochSecond

    val clearableUsers = new ListBuffer[String]()
    mentionTimeouts.foreach(pair =>
    {
      val timeouts = pair._2
      val clearableTimeouts = new ListBuffer[String]()
      timeouts.foreach(timeout =>
      {
        val split = timeout.split(":::")
        if(timeNow - split(1).toInt >= MENTION_TIMEOUT_SEC)
        {
          clearableTimeouts += timeout
        }
      })
      timeouts --= clearableTimeouts

      if(timeouts.isEmpty)
      {
        clearableUsers += pair._1
      }
    })

    mentionTimeouts --= clearableUsers
  }

  def getEmote(guild: Guild, emote: String) : Option[Emote] = {
    val emotes = guild.getEmotesByName(emote, true)
    if(emotes.size > 0)
    {
      return Some(emotes.get(0))
    }
    return None
  }

  def sendGuildNotification(eventKey: String, message: String): Unit = {
    Global.guildEventsToDiscord
      .getOrElse(eventKey, Global.wowToDiscord.getOrElse(
          (ChatEvents.CHAT_MSG_GUILD, None), mutable.Set.empty
        ).map(_._1)
      )
      .foreach(channel => {
        logger.info(s"WoW->Discord(${channel.getName}) $message")
        channel.sendMessage(message).queue()
      })
  }

  def sendAchievementNotification(name: String, achievementId: Int): Unit = {
    val notificationConfig = Global.config.guildConfig.notificationConfigs("achievement")
    if (!notificationConfig.enabled) {
      return
    }

    Global.wowToDiscord.get((ChatEvents.CHAT_MSG_GUILD, None))
      .foreach(_.foreach {
        case (discordChannel, _) =>
          val formatted = notificationConfig
            .format
            .replace("%time", Global.getTime)
            .replace("%user", name)
            .replace("%achievement", messageResolver.resolveAchievementId(achievementId))

          discordChannel.sendMessage(formatted).queue()
      })
  }

  override def onStatusChange(event: StatusChangeEvent): Unit = {
    event.getNewStatus match {
      case Status.CONNECTED =>
        lastStatus.foreach(game => changeStatus(game.getType, game.getName))
        // this is a race condition if already connected to wow, reconnect to discord, and bot tries to send
        // wow->discord message. alternatively it was throwing already garbage collected exceptions if trying
        // to use the previous connection's channel references. I guess need to refill these maps on discord reconnection
        Global.discordToWow.clear
        Global.wowToDiscord.clear
        Global.guildEventsToDiscord.clear

        // getNext seq of needed channels from config
        val configChannels = Global.config.channels.map(channelConfig => {
          channelConfig.discord.channel.toLowerCase -> channelConfig
        })
        val configChannelsNames = configChannels.map(_._1)

        val discordTextChannels = event.getEntity.getTextChannels.asScala
        val eligibleDiscordChannels = discordTextChannels
          .filter(channel =>
            configChannelsNames.contains(channel.getName.toLowerCase) ||
            configChannelsNames.contains(channel.getId)
          )

        // build directional maps
        eligibleDiscordChannels.foreach(channel => {
          configChannels
            .filter {
              case (name, channelConfig) =>
                name.equalsIgnoreCase(channel.getName) ||
                name == channel.getId
            }
            .foreach {
              case (name, channelConfig) =>
                if (channelConfig.chatDirection == ChatDirection.both ||
                  channelConfig.chatDirection == ChatDirection.discord_to_wow) {
                  Global.discordToWow.addBinding(
                    name.toLowerCase, channelConfig.wow
                  )
                }

                if (channelConfig.chatDirection == ChatDirection.both ||
                  channelConfig.chatDirection == ChatDirection.wow_to_discord) {
                  Global.wowToDiscord.addBinding(
                    (channelConfig.wow.tp, channelConfig.wow.channel.map(_.toLowerCase)),
                    (channel, channelConfig.discord)
                  )
                }
            }
          })

        // build guild notification maps
        val guildEventChannels = Global.config.guildConfig.notificationConfigs
          .filter {
            case (key, notificationConfig) =>
              notificationConfig.enabled
          }
          .flatMap {
            case (key, notificationConfig) =>
              notificationConfig.channel.fold[Option[(String, String)]](None)(channel => Some(channel -> key))
          }

        discordTextChannels.foreach(channel => {
          guildEventChannels
            .filter {
              case (name, _) =>
                name.equalsIgnoreCase(channel.getName) ||
                name == channel.getId
            }
            .foreach {
              case (_, notificationKey) =>
                Global.guildEventsToDiscord.addBinding(notificationKey, channel)
            }
        })

        if (Global.discordToWow.nonEmpty || Global.wowToDiscord.nonEmpty) {
          if (firstConnect) {
            discordConnectionCallback.connected
            firstConnect = false
          } else {
            discordConnectionCallback.reconnected
          }
        } else {
          logger.error("No discord channels configured!")
        }
      case Status.DISCONNECTED =>
        discordConnectionCallback.disconnected
      case _ =>
    }
  }

  override def onGuildReady(event: GuildReadyEvent): Unit = {
    checkRequiredDiscordRoles(event.getGuild)

    if(!event.getGuild.getSelfMember.hasPermission(Permission.MANAGE_ROLES)) return

    // Register commands for role management.
    event.getGuild.upsertCommand("addrole", "Add a chatbot role for yourself.")
      .addOption(OptionType.ROLE, "role", "role to add", true)
      .queue()

    event.getGuild.upsertCommand("removerole", "Remove a chatbot role from yourself.")
      .addOption(OptionType.ROLE, "role", "role to remove", true)
      .queue()
  }

  override def onShutdown(event: ShutdownEvent): Unit = {
    event.getCloseCode match {
      case CloseCode.DISALLOWED_INTENTS =>
        logger.error("Per new Discord rules, you must check the PRESENCE INTENT and SERVER MEMBERS INTENT boxes under \"Privileged Gateway Intents\" for this bot in the developer portal. You can find more info at https://discord.com/developers/docs/topics/gateway#privileged-intents")
      case _ =>
    }
  }

  override def onMessageReceived(event: MessageReceivedEvent): Unit = {
    // ignore messages received from self
    if (event.getAuthor.getIdLong == jda.getSelfUser.getIdLong) {
      return
    }

    // ignore messages from non-text channels
    if (event.getChannelType != ChannelType.TEXT) {
      return
    }

    // ignore non-default messages
    val messageType = event.getMessage.getType
    if (messageType != MessageType.DEFAULT && messageType != MessageType.INLINE_REPLY) {
      return
    }

    val channel = event.getChannel
    val channelId = channel.getId
    val channelName = event.getChannel.getName.toLowerCase
    val effectiveName = event.getMember.getEffectiveName
    val message = (sanitizeMessage(event.getMessage.getContentDisplay) +: event.getMessage.getAttachments.asScala.map(_.getUrl))
      .filter(_.nonEmpty)
      .mkString(" ")
    val enableCommandsChannels = Global.config.discord.enableCommandsChannels
    logger.debug(s"RECV DISCORD MESSAGE: [${channel.getName}] [$effectiveName]: $message")

    if ((enableCommandsChannels.nonEmpty && !enableCommandsChannels.contains(channelName)) || !CommandHandler(channel, message)) {
      // send to all configured wow channels
      Global.discordToWow
        .get(channelName)
        .fold(Global.discordToWow.get(channelId))(Some(_))
        .foreach(_.foreach(channelConfig => {
          val finalMessages = if (shouldSendDirectly(message)) {
            Seq(message)
          } else {
            splitUpMessage(channelConfig.format, effectiveName, message)
          }

          finalMessages.foreach(finalMessage => {
            val filter = shouldFilter(channelConfig.filters, finalMessage)
            logger.info(s"${if (filter) "FILTERED " else ""}Discord->WoW(${
              channelConfig.channel.getOrElse(ChatEvents.valueOf(channelConfig.tp))
            }) $finalMessage")
            if (!filter) {
              Global.game.fold(logger.error("Cannot send message! Not connected to WoW!"))(handler => {
                handler.sendMessageToWow(channelConfig.tp, finalMessage, channelConfig.channel)
              })
            }
          })
        }))
    }
  }

  override def onSlashCommand(event: SlashCommandEvent) : Unit = {
    val command = event.getName.toLowerCase
    if(!command.equals("addrole") && !command.equals("removerole")) return
    val self = event.getGuild.getSelfMember

    // Only one chatbot should have this permission.
    if(!self.hasPermission(Permission.MANAGE_ROLES)) return

    if(event.getOptions.isEmpty) return

    val role = event.getOptions.get(0).getAsRole
    if(!isValidChatbotRole(role)) {
      event.reply("Invalid role.").setEphemeral(true).queue()
      return
    }

    if(command.equals("addrole")) {
      if(event.getMember.getRoles.contains(role))
      {
        event.reply("You already have that role.").setEphemeral(true).queue()
        return
      }
      event.getGuild.addRoleToMember(event.getMember, role).queue()
      event.reply("Role added.").setEphemeral(true).queue()
    }
    else {
      if(!event.getMember.getRoles.contains(role))
        {
          event.reply("You don't have that role.").setEphemeral(true).queue()
          return
        }
      event.getGuild.removeRoleFromMember(event.getMember, role).queue()
      event.reply("Role removed.").setEphemeral(true).queue()
    }
  }

  def isValidChatbotRole(role: Role) : Boolean = {
    roles.foreach(registeredRole =>
    {
      if(registeredRole.tagName.equals(role.getName)) //Verify that the requested role is safe
      {
        return true
      }
    })
    return false
  }

  def shouldSendDirectly(message: String): Boolean = {
    val discordConf = Global.config.discord
    val trimmed = message.drop(1).toLowerCase

    message.startsWith(".") &&
    discordConf.enableDotCommands &&
      (
        discordConf.dotCommandsWhitelist.isEmpty ||
        discordConf.dotCommandsWhitelist.contains(trimmed) ||
        // Theoretically it would be better to construct a prefix tree for this.
        !discordConf.dotCommandsWhitelist.forall(item => {
          if (item.endsWith("*")) {
            !trimmed.startsWith(item.dropRight(1).toLowerCase)
          } else {
            true
          }
        })
      )
  }

  def shouldFilter(filtersConfig: Option[FiltersConfig], message: String): Boolean = {
    filtersConfig
      .fold(Global.config.filters)(Some(_))
      .exists(filters => filters.enabled && filters.patterns.exists(message.matches))
  }

  def sanitizeMessage(message: String): String = {
    EmojiParser.parseToAliases(message, EmojiParser.FitzpatrickAction.REMOVE)
  }

  def splitUpMessage(format: String, name: String, message: String): Seq[String] = {
    val retArr = mutable.ArrayBuffer.empty[String]
    val maxTmpLen = 255 - format
      .replace("%time", Global.getTime)
      .replace("%user", name)
      .replace("%message", "")
      .length

    var tmp = message
    while (tmp.length > maxTmpLen) {
      val subStr = tmp.substring(0, maxTmpLen)
      val spaceIndex = subStr.lastIndexOf(' ')
      tmp = if (spaceIndex == -1) {
        retArr += subStr
        tmp.substring(maxTmpLen)
      } else {
        retArr += subStr.substring(0, spaceIndex)
        tmp.substring(spaceIndex + 1)
      }
    }

    if (tmp.nonEmpty) {
      retArr += tmp
    }

    retArr
      .map(message => {
        val formatted = format
          .replace("%time", Global.getTime)
          .replace("%user", name)
          .replace("%message", message)

        // If the final formatted message is a dot command, it should be disabled. Add a space in front.
        if (formatted.startsWith(".")) {
          s" $formatted"
        } else {
          formatted
        }
      })
  }
}
