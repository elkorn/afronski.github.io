module Jekyll

  class TagIndex < Page
    def initialize(site, base, dir, tag)
      @site = site
      @base = base
      @dir = dir
      @name = 'index.html'

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'index_tag.html')

      self.data['tag'] = tag

      tag_title_prefix = site.config['tag_title_prefix'] || 'Tag: '
      self.data['title'] = "#{tag_title_prefix}#{tag}"
    end
  end

  class TagFeed < Page
    def initialize(site, base, dir, tag)
      @site = site
      @base = base
      @dir = dir
      @name = 'feed.xml'

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'index_tag_feed.xml')

      self.data['tag'] = tag

      tag_title_prefix = site.config['tag_title_prefix'] || 'Tag: '
      self.data['title'] = "#{tag_title_prefix}#{tag}"

      self.data['tagged_posts'] = site.posts
        .select { |p| p.tags.include? tag }
        .map { |p| p.render(site.layouts, site.site_payload); p }
        .sort_by { |p| -p.date.to_i }
    end
  end

  class TagGenerator < Generator
    safe true
    priority :lowest

    def generate(site)
      if site.layouts.key? 'index_tag'
        dir = site.config['tag_dir'] || 'tag'
        site.tags.keys.each do |tag|
          write_tag_index(site, File.join(dir, tag), tag)
          write_tag_feed(site, File.join(dir, tag), tag)
        end
      end
    end

    def write_tag_index(site, dir, tag)
      index = TagIndex.new(site, site.source, dir, tag)
      index.render(site.layouts, site.site_payload)
      index.write(site.dest)

      site.pages << index
    end

    def write_tag_feed(site, dir, tag)
      feed = TagFeed.new(site, site.source, dir, tag)
      feed.render(site.layouts, site.site_payload)
      feed.write(site.dest)

      site.pages << feed
    end
  end

end
