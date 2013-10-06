# Jekyll Emoji
#
# Chris Kempson (http://chriskempson.com)
# https://github.com/chriskempson/jekyll-emoji

require 'gemoji'

module Jekyll

  module EmojiFilter

    def emojify(content)
      return false if !content

      config = @context.registers[:site].config
      if config['emoji_dir']
        emoji_dir = config['emoji_dir']
      else
        emoji_dir = "/images/emoji"
      end

      content.to_str.gsub(/:([a-z0-9\+\-_]+):/) do |match|
        if Emoji.names.include?($1) and emoji_dir
          '<img alt="' + $1 + '" src="' + emoji_dir + "/#{$1}.png" + '" class="emoji" />'
        else
          match
        end
      end
    end

  end

  class EmojiGenerator < Generator
    def generate(site)
      config = site.config
      return false if not config['emoji_dir']
      return false if config['emoji_dir'].start_with?('http')
      emoji_dir = File.join(config['source'], config['emoji_dir'])
      return false if File.exist?(emoji_dir + '/smiley.png')

      # Make Emoji directory
      FileUtils.mkdir_p(emoji_dir)

      # Copy Gemoji files
      Dir["#{Emoji.images_path}/emoji/*.png"].each do |src|
        FileUtils.cp src, emoji_dir
      end
    end
  end

end

Liquid::Template.register_filter(Jekyll::EmojiFilter)