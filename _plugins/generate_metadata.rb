module Jekyll
  class IndexMetadataGenerator < Generator
    safe true

    def generate(site)
      site.posts.each { |post| post.data['sorted_tags'] = post.tags.sort }

      indexes = site.pages.select { |page| page.name == 'categories.html' || page.name == 'tags.html' }

      indexes.each do |index|
        index.data['sorted_tags'] = site.tags.map { |tag| tag.first }.sort
        index.data['sorted_categories'] = site.categories.map { |category| category.first }.sort
      end
    end
  end
end
