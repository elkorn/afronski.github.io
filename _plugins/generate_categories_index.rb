module Jekyll

  class CategoryIndex < Page
    def initialize(site, base, dir, category)
      @site = site
      @base = base
      @dir = dir
      @name = 'index.html'

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'index_category.html')

      self.data['category'] = category

      category_title_prefix = site.config['category_title_prefix'] || 'Category: '
      self.data['title'] = "#{category_title_prefix}#{category}"
    end
  end

  class CategoryFeed < Page
    def initialize(site, base, dir, category)
      @site = site
      @base = base
      @dir = dir
      @name = 'feed.xml'

      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'index_category_feed.xml')

      self.data['category'] = category

      category_title_prefix = site.config['category_title_prefix'] || 'Category: '
      self.data['title'] = "#{category_title_prefix}#{category}"

      self.data['posts_with_that_category'] = site.posts
        .select { |p| p.categories.include? category }
        .map { |p| p.render(site.layouts, site.site_payload); p }
    end
  end

  class CategoryGenerator < Generator
    safe true
    priority :lowest

    def generate(site)
      if site.layouts.key? 'index_category'
        dir = site.config['category_dir'] || 'category'
        site.categories.keys.each do |category|
          write_category_index(site, File.join(dir, category), category)
          write_category_feed(site, File.join(dir, category), category)
        end
      end
    end

    def write_category_feed(site, dir, category)
      feed = CategoryFeed.new(site, site.source, dir, category)
      feed.render(site.layouts, site.site_payload)
      feed.write(site.dest)

      site.pages << feed
    end

    def write_category_index(site, dir, category)
      index = CategoryIndex.new(site, site.source, dir, category)
      index.render(site.layouts, site.site_payload)
      index.write(site.dest)

      site.pages << index
    end
  end

end
