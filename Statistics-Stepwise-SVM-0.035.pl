#!/usr/bin/perl

use strict;
use warnings;
use File::Copy;
use File::Basename;
use Statistics::R;
use List::Util 'shuffle';

my $directory = $ARGV[0];
my $dir = dirname($directory);
my $file = basename($directory);

my $train_start = $ARGV[1];
my $train_end = $ARGV[2];
my $test_start = $ARGV[3];
my $test_end = $ARGV[4];
my $step = 0;

open DATA,"<","$dir/$file" or die "$!";
chomp(my $data = <DATA>);
close DATA;
my @data = split /\t/,$data;
my $total = scalar(@data);
copy("$dir/$file","$dir/data_$total.txt") or die "$!";

open LOG,">","$dir/log.txt" or die "$!";
my ($input,$output);
my (%Column_Desc,%Column_ACC,%Column_MCC,%Column_SP,%Column_SE,%Column_Score);
while($total > $step + 2){
    $input = "data_"."$total.txt";
    print LOG "$input:\n";
    print "$input:\n";
    print "Column\tDesc\tACC\tMCC\tSP\tSE\tScore\n";
    $step = 4 if $total >= 2**8;
	$step = 3 if $total < 2**8;
	$step = 2 if $total < 2**7;
	$step = 1 if $total < 2**6;
    my $drop = $total - $step;
    $output = "data_"."$drop.txt";
    open IN,"<","$dir/$input" or die "$!";
    chomp(my $in = <IN>);
    my @in = split /\t/,$in;
    %Column_Desc = ();
    for my $col (0..@in - 1){
        $Column_Desc{$col + 1} = $in[$col];
    }
    %Column_ACC = ();
    %Column_MCC = ();
    %Column_SP = ();
    %Column_SE = ();
    %Column_Score = ();
    for my $count (3..$total){
        print "$count\t$Column_Desc{$count}\t";
        &drop($count);
        &R($count);
    }
    my @keys = sort by_Score shuffle keys %Column_ACC;
#    my @keys = sort by_ACC_MCC_SP_SE keys %Column_ACC;
    print LOG "sort:\n";
    print "sort:\n";
    print LOG "Column\tDesc\tACC\tMCC\tSP\tSE\tScore\n";
    for(@keys){print LOG "$_\t$Column_Desc{$_}\t$Column_ACC{$_}\t$Column_MCC{$_}\t$Column_SP{$_}\t$Column_SE{$_}\t$Column_Score{$_}\n";}
    for(@keys){print "$_\t$Column_Desc{$_}\t$Column_ACC{$_}\t$Column_MCC{$_}\t$Column_SP{$_}\t$Column_SE{$_}\t$Column_Score{$_}\n";}
    print LOG "drop:\n";
    print "drop:\n";
    my @drops = ();
    for my $index (0..$step - 1){
        push @drops,$keys[$index];
        print LOG "$keys[$index]\t$Column_Desc{$keys[$index]}\n";
        print "$keys[$index]\t$Column_Desc{$keys[$index]}\n";
    }
    &drop(@drops);
    $total = $total - $step;
}
sub R {
    my $col = shift;
    my $R = Statistics::R -> new();
    $R -> startR;
    $R -> send(qq'library(class)');
    $R -> send(qq'library(e1071)');
    $R -> send(qq'svmdata <- read.delim("$dir/$output",header = TRUE,sep = "\t")');
    $R -> send(qq'n <- length(svmdata)');
    $R -> send(qq'trainset <- svmdata[$train_start:$train_end,3:n]');
    $R -> send(qq'trainlabel <- svmdata[$train_start:$train_end,2]');
    $R -> send(qq'testset <- svmdata[$test_start:$test_end,3:n]');
    $R -> send(qq'testlabel <- svmdata[$test_start:$test_end,2]');
    $R -> send(qq'model <- svm(trainset,trainlabel,type="C-classification")');
    $R -> send(qq'predlabel <- predict(model,testset)');
    $R -> send(qq'result <- table(predlabel,testlabel)');
    $R -> send(qq'TP <- result[2,2]');
    $R -> send(qq'TN <- result[1,1]');
    $R -> send(qq'FP <- result[1,2]');
    $R -> send(qq'FN <- result[2,1]');
    $R -> send(qq'ACC <- (TP+TN)/(TP+TN+FP+FN)');
    $R -> send(qq'print(ACC)');
    my $read = $R -> read;
    $read = &format($read);
    $Column_ACC{$col} = $read;
    print "$read\t";
    $R -> send(qq'MCC <- (TP*TN-FN*FP)/sqrt((TP+FN)*(TP+FP)*(TN+FN)*(TN+FP))');
    $R -> send(qq'print(MCC)');
    $read = $R -> read;
    my @read = split/\s+/,$read;
    $read = sprintf "%.3f", $read[1];
    $Column_MCC{$col} = $read;
    print "$read\t";
    $R -> send(qq'SP <- TN/(TN+FP)');
    $R -> send(qq'print(SP)');
    $read = $R -> read;
    $read = &format($read);
    $Column_SP{$col} = $read;
    print "$read\t";
    $R -> send(qq'SE <- TP/(TP+FN)');
    $R -> send(qq'print(SE)');
    $read = $R -> read;
    $read = &format($read);
    $Column_SE{$col} = $read;
    print "$read\t";
    $Column_Score{$col} = ($Column_ACC{$col} + $Column_MCC{$col})*($Column_SP{$col} + $Column_SE{$col})/2;
    print "$Column_Score{$col}\n";
    $R -> stopR();
}
sub format {
    my $figure = shift;
    my @figure = split/\s+/,$figure;
    $figure = sprintf "%.2f", $figure[1]*100;
}
sub by_ACC_MCC_SP_SE {
    $Column_ACC{$b} <=> $Column_ACC{$a}
    or
    $Column_MCC{$b} <=> $Column_MCC{$a}
    or
    $Column_SP{$b} <=> $Column_SP{$a}
    or
    $Column_SE{$b} <=> $Column_SE{$a}
}
sub by_Score {
                $Column_Score{$b} <=> $Column_Score{$a}
}
sub drop {
    my @drops = @_;
    open IN,"<","$dir/$input" or die "$!";
    open OUT,">","$dir/$output" or die "$!";
    while(<IN>){
        chomp;
        my @data = split /\t/;
        my $out = $data[0];
        for my $index (1..@data - 1){
            my $dropcount = 0;
            foreach my $drop (@drops){
                $dropcount++ if $index + 1 == $drop;
            }
            $out = join("\t",$out,$data[$index]) if $dropcount == 0;
                          }
        print OUT "$out\n";
    }
    close IN;
    close OUT;
}