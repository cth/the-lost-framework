#!/usr/bin/env ruby
require './ai4r/lib/ai4r.rb'

include Ai4r

class String
  def to_prolog
    "'" + self.gsub("'",'\\\\\'') + "'"
  end
end

class Array
  def to_prolog
    "[" + (self.map { |e| e.to_prolog rescue e }).join(',') + "]"
  end
end

class GeneClusterer
  def initialize
    @kmeans = Ai4r::Clusterers::KMeans.new
  end

  # load the data set
  def load_gene_data(genes_file, label_file)
    puts "Loading data..."
    @genes = Ai4r::Data::DataSet.new
    @genes.parse_csv(genes_file)
    
    puts "Loading labels"
    labels = []
    File.open(label_file,'r') do |file|
      file.each do |line|
        labels << line.chomp
      end
    end
    @genes.set_data_labels(labels)

    puts "Done loading data..."
  end
  
  def load_genome_data(file)
    @genome = Ai4r::Data::DataSet.new
    @genome.parse_csv(file)
    puts "Done loading genome..."
  end
  
  def add_genome_cluster
    @clusterer
  end

  def cluster_genes(num_clusters)
    puts "Building #{num_clusters} clusters.."
    @clusterer = Ai4r::Clusterers::KMeans.new
    #@clusterer.distance_function = max_distance_function
    @clusterer.build(@genes,num_clusters)
    puts "Done."
  end
  
  def report_distances
    total_distance_all_clusters = 0
    total_deviation = 0
    smallest_null_model_distance = 10000
    0.upto(@clusterer.clusters.size-1) do |i|
      cdist = self.cluster_total_distance(i)
      mdist = self.cluster_mean_distance(cdist,i)
      variance = self.cluster_variance(i)
      standard_deviation = Math.sqrt(variance)
      total_distance_all_clusters = total_distance_all_clusters + cdist
      total_deviation = total_deviation + standard_deviation
      null_model_distance = distance_to_null_model(i)
      smallest_null_model_distance = null_model_distance if null_model_distance < smallest_null_model_distance

      puts "cluster[#{i}]:"
      puts "\tcluster[#{i}].size = #{@clusterer.clusters[i].data_items.size}"
      puts "\tTotal distance for cluster[#{i}] = #{cdist}"
      puts "\tAverage distance for cluster[#{i}] = #{mdist}"
      puts "\tVariance for cluster[#{i}] = #{variance}"
      puts "\tStandard deviation for cluster[#{i}] = #{standard_deviation}"
      puts "\tDistance to null model: #{null_model_distance}"
    end
    puts "-"*60
    puts "Total distance for all clusters " + total_distance_all_clusters.to_s
    puts "Average standard deviation: " + (total_deviation / @clusterer.clusters.size).to_s
    puts "Smallest null model distance: " + smallest_null_model_distance.to_s
    
    centroid_distances
  end
  
  def test_classify
    # Add the null model cluster to the list of clusters
    @clusterer.number_of_clusters = @clusterer.number_of_clusters + 1
    @clusterer.centroids << @genome.data_items.first
    @clusterer.calculate_membership_clusters
    0.upto(@clusterer.clusters.size-1) do |i|
      puts "\tcluster[#{i}].size = #{@clusterer.clusters[i].data_items.size}"
    end
  end
  
  def distance_to_null_model(cluster_id)
      null_model_centroid = @genome.data_items.first
      @clusterer.distance(null_model_centroid,@clusterer.centroids[cluster_id])
  end
  
  def cluster_total_distance(cluster_id)
    total_distance = 0
    data_items = @clusterer.clusters[cluster_id].data_items
    data_items.each do |data_item|
      dist = @clusterer.distance(data_item,@clusterer.centroids[cluster_id])
      total_distance = total_distance + dist
    end
    total_distance
  end
  
  def cluster_mean_distance(total_distance, cluster_id)
    total_distance / @clusterer.clusters[cluster_id].data_items.size
  end
  
  def cluster_variance(cluster_id)
        sum = 0
        data_items = @clusterer.clusters[cluster_id].data_items
        data_items.each do |data_item|
          dist = @clusterer.distance(data_item,@clusterer.centroids[cluster_id])
          dist2 = dist * dist
          sum = sum + dist2
        end
        variance = sum / @clusterer.clusters[cluster_id].data_items.size
  end
  
  def calc_variances
      0.upto(@clusterer.clusters.size-1) do |i|
        sum = 0
        data_items = @clusterer.clusters[i].data_items
        data_items.each do |data_item|
          dist = @clusterer.distance(data_item,@clusterer.centroids[i])
          dist2 = dist * dist
          sum = sum + dist2
        end
        variance = sum / @clusterer.clusters[i].data_items.size
        standard_dev = Math.sqrt(variance)
        puts "cluster[#{i}] variance=#{variance} standard deviation=#{standard_dev}"
      end
  end
  
  def centroid_distances
    average_centroid_distance = 0
    max_centroid_distance = 0
    min_centroid_distance = 10000 # just a large constant
    distances = []
    0.upto(@clusterer.centroids.size-1) do |i|
      0.upto(@clusterer.centroids.size-1) do |j|
        next if i == j # skip self<->self distance
        dist = @clusterer.distance(@clusterer.centroids[i],@clusterer.centroids[j])
        max_centroid_distance = dist if dist > max_centroid_distance
        min_centroid_distance = dist if dist < min_centroid_distance
        distances << dist
      end
    end
    total = 0
    distances.each do |d|
      total = total + d
    end
    average_distance = total.to_f / distances.size.to_f
    puts "Average centroid distance: #{average_distance}"
    puts "Maximal centroid distance: #{max_centroid_distance}"
    puts "Minimal centroid distance: #{min_centroid_distance}"
  end
  
  def max_distance_function
    lambda do |a,b|
      dist = 0.0
      ec_dist = 0.0
      a.each_index do |index|
        if a[index].is_a?(Numeric) && b[index].is_a?(Numeric)
          point_dist = ((a[index]-b[index])*(a[index]-b[index]))
          dist = point_dist if point_dist > dist
          ec_dist = ec_dist + point_dist
        end
      end
      (dist > ec_dist) ? dist : ec_dist
    end
  end

  
  def incremental_clustering(max_clusters)
    1.upto(max_clusters) do |i|
      build_and_test_cluster_of_size(i)      
    end
  end
  
  def build_and_test_cluster_of_size(i)
      cluster_genes(i)
      report_distances
      test_classify
  end
  
  def write_clusters_to_file(clusters_file)
    File.open(clusters_file,"w") do |file|
      0.upto(@clusterer.centroids.size-1) do |i|
        fact = "cluster(#{i+1},"

        cluster_stats_list = []
        1.upto(@genes.data_labels.length-1) do |j|
          lbl = @genes.data_labels[j]
          stat = @clusterer.centroids[i][j]
          cluster_stats_list << "(#{lbl},#{stat})"
        end
        fact << "[" + cluster_stats_list.join(',') + "],"
        genes_in_cluster = []
        @clusterer.clusters[i].data_items.each do |data_item|
          genes_in_cluster << data_item.first
        end
        fact << genes_in_cluster.to_prolog + ")."
        file << fact + "\n"
      end
    end
  end
end

## Main 
if ARGV.length != 3
then
        puts "this script takes exactly three arguments:"
        puts " - The name of a csv file with one line for each gene"
        puts " - The name of a file with labels for of the csv fields"
        puts " - The number of clusters to create"
        exit
end

stats_file = ARGV[0]
label_file = ARGV[1]
num_clusters = ARGV[2]

puts "Stats file: " + stats_file
puts "label file: " + label_file
puts "number of clusters: " + num_clusters

gc = GeneClusterer.new
gc.load_gene_data(stats_file, label_file)
gc.cluster_genes(num_clusters.to_i)
gc.write_clusters_to_file('clusters.pl')

